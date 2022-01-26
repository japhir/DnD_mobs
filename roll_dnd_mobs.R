### Introduction

# We're running a massive final D&D 5e battlefield, and would like to roll the dice!
# but having to roll 20d20 to determine if your army of archers hits is a pain
# so here is some R code to simulate dice-rolls for large groups of mobs.

# It shows you exactly what was rolled so you can still cheer on your army of champions!
# (except for individual damage dice, but I don't care too much about that)

### libraries
## library(tidyverse)
# I'm using glue for messages
# install.packages("glue")

### functions

#' Do a single dice roll
#'
#' this allows you to roll e.g. 3d8 by calling `roll_dice(3, 8)`.
#' @return the total of the two dice
roll_dice <- function(n = 1, d = 20) {
  sum(sample(seq_len(d), size = n, replace = TRUE))
}

#' Roll a d20
#'
#' This is the special case for the d20, that allows you to roll with advantage/disadvantage
#' It returns only the roll, but can show a message with roll results if desired.
d20 <- function(advantage = FALSE, disadvantage = FALSE, verbose = advantage | disadvantage) {
  # roll a d20
  roll1 <- roll_dice(1, 20)

  if (advantage & disadvantage) {
    warning("You have specified both advantage and disadvantage, they cancel out.")
    advantage <- disadvantage <- FALSE
  }

  # roll a second time
  if (advantage | disadvantage) {
    roll2 <- roll_dice(1, 20)
  } else {
    roll2 <- NA
  }

  if (advantage) {
    roll <- max(roll1, roll2)
  } else if (disadvantage) {
    roll <- min(roll1, roll2)
  } else {
    roll <- roll1
  }

  if (verbose) {
    msg <- paste0("with advantage"[advantage], "with disadvantage"[disadvantage])
    ## message(glue::glue("Rolling a d20 {msg}"))
    if (advantage | disadvantage) {
      msg2 <- glue::glue(" and a {roll2}")
    } else {
      msg2 <- ""
    }
    message(glue::glue("Rolled a: {roll1}{msg2}"))
  }

  roll
}

#' Attack with a group of mobs
#'
#' For example, a group of 20 3rd-level rangers with archery have a `to_hit` of 8
#' They deal 1d8 + 4 damage
#' Specify the defender's AC to see if they hit and to total the damage.
attack <- function(n_mobs = 20,
                   to_hit = +8,
                   dmg_die_n = 1,
                   dmg_die = 8,
                   dmg_bonus = +4,
                   AC = 15,
                   advantage = FALSE,
                   disadvantage = FALSE) {
  # do n_mobs d20 rolls
  rolls <- replicate(n_mobs, d20(advantage = advantage,
                                 disadvantage = disadvantage))
  crit_hit <- rolls == 20
  crit_miss <- rolls == 1
  crit_n <- sum(crit_hit)
  crit_miss_n <- sum(crit_miss)
  hits <- rolls + to_hit >= AC # for AC it's "equals or exceeds" PHB 194
  hits[crit_hit] <- TRUE # a critical hit ALWAYS hits, regardless of the AC
  hits[crit_miss] <- FALSE # a critical miss ALWAYS misses, regardless of the AC
  hit_n <- sum(hits)

  # do damage rolls
  dmg_n <- hit_n + crit_n # we add damage dice rolls for critical hits
  dmg <- replicate(dmg_n, roll_dice(dmg_die_n, dmg_die) + dmg_bonus)
  tot <- sum(dmg)
  list(d20 = rolls,
       hit = rolls + to_hit,
       crit_n = crit_n,
       crit_miss_n = crit_miss_n,
       hit_n = hit_n,
       dmgn = dmg_n,
       dmg = dmg,
       tot = tot)
}

#' Roll saving throws for a group of mobs
#'
#' For example, with a group of 20 archers who are proficient in DEX saves,
#' they roll d20+6 against the DC.
save <- function(n_mobs = 20,
                 skill_bonus = +6,
                 DC = 15,
                 advantage = FALSE,
                 disadvantage = FALSE) {
  rolls <- replicate(n_mobs, d20(advantage = advantage,
                                 disadvantage = disadvantage))
  success <- rolls + skill_bonus >= DC # equal or exceed = PHB page 7 (unless contested roll)
  success_n <- sum(success)
  list(d20 = rolls, check = rolls + skill_bonus, success_n = success_n)
}

# a synonym for a skill save
#' Roll skill checks for a group of mobs
check <- save
