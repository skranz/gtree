# Convert a table form game into a gambit extensive form game

get.xs = function(xs = app[["xs"]],app = if ("package:shinyEvents" %in% search()) getApp()) {
	xs
}

gtree.stop.on.error = function(..., xs=get.xs()) {
  is.null(xs)
}

# Get the gtree project directory
#
# If the gtreeGUI is running use the project.dir specified
# there.
# Otherwise check getOptions("gtree.project.dir).
# If that is null return the current working directory
# @export
get.project.dir = function(xs=get.xs()) {
	if (!is.null(xs$project.dir)) return(xs$project.dir)
	project.dir = getOption("gtree.project.dir")
	if (!is.null(project.dir)) return(project.dir)
  getwd()
}


# Set the gtree project directory
#
# By default the current working directory
# @export
set.project.dir = function(project.dir=getwd(), xs=get.xs()) {
  if (!is.null(xs)) {
    xs$project.dir = project.dir
  }
  options(gtree.project.dir=project.dir)
}

# Get the gtree games directory
#
# The subdirectory "games" under get.project.dir()
# @export
get.games.dir = function(project.dir = get.project.dir()) {
	file.path(project.dir,"games")
}

# Get the gtree jobs directory
#
# The subdirectory "jobs" under get.project.dir()
# @export
get.jobs.dir = function(project.dir = get.project.dir()) {
	file.path(project.dir,"jobs")
}


get.game.dir = function(gameId, project.dir=get.project.dir()) {
	file.path(project.dir,"games",gameId)
}

get.eq.dir = function(gameId, project.dir = get.project.dir()) {
	file.path(project.dir,"games",gameId,"eq")
}

get.efg.dir = function(gameId, project.dir = get.project.dir()) {
	file.path(project.dir,"games",gameId,"gambit")
}

get.pages.dir = function(gameId, project.dir = get.project.dir()) {
	file.path(project.dir,"games",gameId,"pages")
}

# Create a directory structure for a new game
#
# includes the subdirectories eq, gambit and pages
# @export
make.game.dir = function(gameId, games.dir = file.path(project.dir,"games"), project.dir=get.project.dir()) {
	if (length(gameId)!=1) return()
	if (nchar(gameId)==0) return()
	game.dir = file.path(games.dir, gameId)

	if (dir.exists(game.dir)) return()

	dir.create(game.dir)
	dir.create(file.path(game.dir,"eq"))
	dir.create(file.path(game.dir,"gambit"))
	dir.create(file.path(game.dir,"pages"))
}

get.jg.hash = function(jg.hash=NULL, jg=NULL, rg=NULL,vg=NULL, tg=NULL) {
	hash = first.non.null(jg.hash, rg$jg.hash, vg$jg.hash, tg$jg.hash)
	if (is.null(hash) & !is.null(jg)) hash = digest(jg)
	hash
}

# Get a game in jg format by its gameId
#
# Looks in the current project directory by default
# and parses the json file
get.jg = function(gameId,json.file = paste0(game.dir,"/",gameId,".json"), game.dir=file.path(games.dir,gameId), games.dir = get.games.dir(project.dir), project.dir = get.project.dir(), jg=NULL) {
	restore.point("get.jg")
	if (!is.null(jg)) return(jg)

	json = merge.lines(readLines(json.file,warn = FALSE))
  content = fromJSON(json,simplifyDataFrame = FALSE,simplifyMatrix = FALSE,simplifyVector = FALSE)
	content$game
}

# Get a game in rg format by its gameId
#
# If the json file has not changed return old .rg file
# otherwise generate new rg file from json source
get.rg = function(gameId = jg$gameId, jg.hash = get.jg.hash(jg=jg),jg=NULL,rg=NULL, games.dir = get.games.dir(project.dir), project.dir = get.project.dir(), save.new = TRUE) {
	if (!is.null(rg)) return(rg)
	restore.point("get.rg")
	if (is.null(jg.hash)) {
		jg = get.jg(gameId = gameId, games.dir = games.dir)
		jg.hash = get.jg.hash(jg=jg)
	}


	file = file.path(games.dir,gameId, paste0(gameId,".rg"))
	if (file.exists(file)) {
		# return old rg if jg.hash has not changed
		rg = readRDS(file)
		if (identical(rg$jg.hash, jg.hash) | is.null(jg.hash))
			return(rg)
	}

	# need to create new rg
	if (is.null(jg)) jg = get.jg(gameId=gameId, games.dir = games.dir)

	rg = jg.to.rg(jg)
	if (save.new) {
		saveRDS(rg, file)
	}
	rg
}

# Get a game in vg format by its gameId
#
# If the json file has not use exsisting .rg file to extract
# the vg format.
# Otherwise first generate new rg files from json source
get.vg = function(variant=1, gameId = first.non.null(jg$gameId,rg$gameId), jg.hash = get.jg.hash(jg=jg, rg=rg),jg=NULL,rg=NULL, vg=NULL, games.dir = get.games.dir(project.dir), project.dir = get.project.dir(), save.new = FALSE, always.new=FALSE) {
	if (!is.null(vg)) return(vg)
	restore.point("get.vg")
	if (is.null(jg.hash)) {
		jg = get.jg(gameId = gameId, games.dir = games.dir)
		jg.hash = get.jg.hash(jg=jg)
	}

	if (is.numeric(variant)) {
		rg = get.rg(gameId=gameId, jg=jg, rg=rg, jg.hash=jg.hash, games.dir=games.dir)
		variant = rg$variants[[variant]]
	}


	file = file.path(games.dir,gameId, paste0(gameId,"_",variant, ".vg"))
	if (file.exists(file) & !always.new) {
		# return old vg if jg.hash has not changed
		vg = readRDS(file)
		if (identical(vg$jg.hash, jg.hash) | is.null(jg.hash))
			return(vg)
	}

	rg = get.rg(gameId=gameId, jg=jg, rg=rg, jg.hash=jg.hash, games.dir=games.dir)

	vg = rg.to.vg(rg,variant = variant)
	if (save.new) {
		saveRDS(vg, file)
	}
	vg
}

# Load tg without any checks
load.tg = function(variant=first.non.null(vg$variant,1), gameId = first.non.null(vg$gameId,jg$gameId,rg$gameId),jg=NULL,rg=NULL, vg=NULL, tg=NULL, tg.id=tg$tg.id, games.dir = get.games.dir(project.dir), project.dir = get.project.dir(), filename= if(!is.null(tg.id)) paste0(tg.id,".tg") else NULL) {

	if (is.null(filename))
	  filename = paste0(gameId,"_",variant, ".tg")

	file = file.path(games.dir,gameId, filename)
	tg = readRDS(file)
  tg
}

# Get a game in tg format by its gameId
#
# If the json file has not changed return old .tg file
# otherwise generate new rg and tg files from json source
get.tg = function(variant=first.non.null(vg$variant,1), gameId = first.non.null(vg$gameId,jg$gameId,rg$gameId), jg.hash = get.jg.hash(jg=jg, rg=rg,vg=vg),jg=NULL,rg=NULL, vg=NULL, tg=NULL, games.dir = get.games.dir(project.dir), project.dir = get.project.dir(), save.new = TRUE,branching.limit = 10000,msg.fun=NULL, never.load = FALSE, filename=NULL) {
	if (!is.null(tg)) return(tg)
	restore.point("get.tg")

	if (is.null(jg.hash)) {
		jg = get.jg(gameId = gameId, games.dir = games.dir)
		jg.hash = get.jg.hash(jg=jg)
	}

	if (is.numeric(variant)) {
		rg = get.rg(gameId=gameId, jg=jg, rg=rg, jg.hash=jg.hash, games.dir=games.dir)
		variant = rg$variants[[variant]]
	}


	if (is.null(filename))
	  filename = paste0(gameId,"_",variant, ".tg")

	file = file.path(games.dir,gameId, filename)
	if (file.exists(file) & !never.load) {
		# return old vg if jg.hash has not changed
		tg = readRDS(file)
		if (identical(tg$jg.hash, jg.hash) | is.null(jg.hash))
			return(tg)
	}

	vg = get.vg(variant=variant, gameId=gameId, jg=jg, rg=rg, vg=vg, jg.hash=jg.hash, games.dir=games.dir)

	tg = vg.to.tg(vg,branching.limit = branching.limit, msg.fun=msg.fun)
	if (tg$kel$count>0) {
		return(tg)
	}
	if (save.new) {
		if (!is.null(msg.fun)) msg.fun("Save game tree in ",file,"...")
		saveRDS(tg, file)
		if (!is.null(msg.fun)) {
			msg.fun("Game tree saved as ",file, ".\n Size = ", file.size(file))
		}
	}
	tg
}

save.rg = function(rg, games.dir = get.games.dir(project.dir), project.dir = get.project.dir()) {
	gameId = rg$gameId
	make.game.dir(gameId,games.dir = games.dir)

	file = file.path(games.dir,rg$gameId, paste0(rg$gameId, ".rg"))
	saveRDS(rg, file)
}

save.vg = function(vg, games.dir = get.games.dir(project.dir), project.dir = get.project.dir()) {
	gameId = vg$gameId
	make.game.dir(gameId,games.dir = games.dir)

	file = file.path(games.dir,gameId, paste0(gameId,"_",vg$variant, ".vg"))
	saveRDS(vg, file)
}


save.tg = function(tg, games.dir = get.games.dir(project.dir), project.dir = get.project.dir(), filename = paste0(str.left.of(tg$tg.id,"__"),".tg")) {
	gameId = tg$gameId
	make.game.dir(gameId,games.dir = games.dir)

	file = file.path(games.dir,gameId, filename)
	saveRDS(tg, file)
}

has.uptodate.eq = function(tg,util.funs=NULL, just.spe=TRUE, mixed=FALSE, eq.dir = get.eq.dir(tg$gameId,project.dir), efg.dir = get.efg.dir(tg$gameId, project.dir), project.dir = get.project.dir(), save.new = TRUE, solvemode=NULL, solver=NULL, ...) {

  if (!is.null(util.funs))
		set.tg.util(tg=tg,util.funs)

	eq.id = get.eq.id(tg=tg, just.spe=just.spe, mixed=mixed, solvemode=solvemode)
	file = file.path(eq.dir, paste0(eq.id,".eq"))
	if (file.exists(file)) {
		# return old vg if jg.hash has not changed
		eq = readRDS(file)
		if (identical(tg$jg.hash, eq$jg.hash))
			return(TRUE)
	}
  return(FALSE)

}

# Compute or return previously computed equilibria
#
# Uses cached equilibria if json file of the game has not
# changed. Otherwise solve new equilibrium via gambit.eq.solve
get.eq = function(tg, util.funs=NULL, just.spe=TRUE, mixed=FALSE, eq.dir = get.eq.dir(tg$gameId,project.dir), efg.dir = get.efg.dir(tg$gameId, project.dir), project.dir = get.project.dir(), save.new = TRUE, solvemode=NULL, solver=NULL, only.load=FALSE,create.efg.even.for.only.load=TRUE,never.load=FALSE,...) {
	restore.point("get.eq")
	if (!is.null(util.funs))
		set.tg.util(tg=tg,util.funs)


	eq.id = get.eq.id(tg=tg, just.spe=just.spe, mixed=mixed, solvemode=solvemode)
	file = file.path(eq.dir, paste0(eq.id,".eq"))
	if (file.exists(file) & !never.load) {
		# return old vg if jg.hash has not changed
		eq = readRDS(file)
		if (identical(tg$jg.hash, eq$jg.hash))
			return(eq$eq.li)
	}

	# create up-to-date efg file even if only.load = TRUE
	if (!only.load | create.efg.even.for.only.load)
	  tg.to.efg(tg=tg, path=efg.dir)

	if (only.load) return(NULL)



	# solve equilibrium
	eq.li = gambit.solve.eq(tg, just.spe=just.spe, mixed=mixed,eq.dir=eq.dir,save.eq = save.new,solver=solver,solvemode=solvemode,...)
	eq.li
}



example.remove.all.tg = function() {
  games.dir = "D:/libraries/gtree/myproject/games"
  remove.all.tg(games.dir)

}

# Removes all tg files of all games in games.dir.
# Can be useful, if we changed the definition
# of a tg and want to update all games
remove.all.tg = function(games.dir) {
  restore.point("remove.all.tg")
  games = list.dirs(games.dir,full.names = FALSE,recursive = FALSE)
  for (game in games) {
    dir = file.path(games.dir, game)
    tg.files = list.files(dir, glob2rx("*.tg"),full.names = TRUE)
    file.remove(tg.files)
  }

}
