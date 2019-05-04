## Text adventure in R
## The Devil is in the Data
## lucidmanager.org

## Initiate game
verbs <- c("look", "take", "put", "use", "wait", "kill", "inventory", "help")
moves <- c("north", "south", "east", "west", "up", "down")
rooms <- read.csv("rooms.csv")
objects <- read.csv("objects.csv")
actions <- readLines("actions.txt")
room <- 1
resistance <- 6

## Displays relevant prose (descriptions and results of actions)
prose <- function(ref) {
    line <- which(actions == paste("#", ref)) + 1
    if(length(line) == 0)
        return()
    while(substr(actions[line], 1, 1) != "#") {
        print(actions[line])
        line <- line + 1
    }
}

## Display items the player is carrying
inventory <- function() {
    stuff <- subset(objects, type == "object" & location == 0)$name
    if (length(stuff) == 0)
        stuff <- "nothing"
    print(paste0("You have ", paste(stuff, collapse = ", "), "."))
}

## Describe the local surroundings
look <- function(room) {
    ## Describe
    prose(paste("room", room))
    ## Directions
    passages <- moves[which(rooms[rooms$room == room, 3:8] != 0)]
    print(paste0("You can go: ", paste(passages, collapse = ", "), "."))
    ## Objects
    stuff <- subset(objects, type == "object" & location == room)$name
    if (length(stuff) != 0)
        print(paste0("You see: ", paste(stuff, collapse = ", "), "."))
    ## Inventory
    inventory()
}

## Take an object
take <- function(object) {
    if (is.na(object))
        return(print("You cannot take this."))
    stuff <- subset(objects, type == "object" & location == room)$name
    if (object %in% stuff) {
        objects[which(objects$name == object), "location"] <<- 0
        print(paste0("You take the ", object, "."))
        inventory()
        }
    else
        print(paste0("You cannot take the ", object, "."))    
}

## Put and object on the ground
put <- function(object)  {
    stuff <- subset(objects, type == "object" & location == 0)$name
    if (object %in% stuff) {
        objects[which(objects$name == object), "location"] <<- room
        print(paste0("You place the ", object, " on the ground."))
        inventory()
        }
    else
        print(paste0("You don't have the ", object, "."))    
}

## Use an object
use <- function(object) {
}

## Wait for situation to change
wait <- function(dummy) {
}

## KIll something
kill <- function(object) {
}

## Move player
walk <- function(direction) {
    r <- rooms[rooms$room == room, direction]
    if (r != 0) {
        room <<- r
        rooms$visited[room] <<- TRUE
    }
    else
        print("You can't go that way.")
    look(room)
    #carry <- subset(objects, type == "object" & room == room)$name    
}

## Display help
help <- function(dummy) {
    prose("help")
    print(paste0("You can use the following verbs: ", paste (verbs, collapse = ", "), "."))
}

## Game Play loop
while (resistance > 0) {
    verb <- NA
    move <- NA
    object <- NA
    command <- readline(prompt = "What would you like to do? :")
    command <- tolower(command)
    words <- unlist(strsplit(command, " "))
    verb <- verbs[verbs %in% words][1] # First valid verb in the list
    direction <- moves[moves %in% words][1] # First valid direction in the list
    object <- objects$name[objects$name %in% words][1] # First valid object
    if (!is.na(direction)) {
        walk(direction)
    }
    if (!is.na(verb)) {
        if (verb == "look")
            arg <- room
        else
            arg <- object
        do.call(verb, list(arg))
    }
    if (!is.na(direction) & !is.na(verb))
        print("You are talking gibberish.")
}

## Finalise game
prose(ifelse(resistance <= 0, "Death", "Victory"))



rooms






## Visualise map
connections <- data.frame(from = rep(NA, nrow(rooms) * 6),
                          to = NA)
library(tidyr)
library(igraph)
edges <- rooms %>%
    select(room, north, south, east, west) %>%
    gather("direction", "to", -1) %>%
    select(from = room, to, direction) %>%
    filter(to != 0)
connections <- as.matrix(edges[, 1:2])
g <- graph_from_edgelist(connections)
plot(g, layout = layout.grid)

