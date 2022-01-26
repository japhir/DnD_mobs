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

### some examples

# attack with 15 human hunters with a shortsword.
attack(15, +6, 1, 8, +4, AC = 15)
#> $d20
#>  [1] 20  1 19  6  6 10 10  8  7 17  1 11 16 13 11

#> $hit
#>  [1] 26  7 25 12 12 16 16 14 13 23  7 17 22 19 17

#> $crit_n
#> [1] 1

#> $crit_miss_n
#> [1] 2

#> $hit_n
#> [1] 9

#> $dmgn
#> [1] 10

#> $dmg
#>  [1]  8  5 11  7  9  9 10 12 12 10

#> $tot
#> [1] 93

# they can also attack with their off-hand using their bonus action, but for
# the damage we do not add the ability modifier
attack(15, +6, 1, 8, +0, AC = 15)

# our hunters are colossus slayers, so they deal an extra 1d8 damage if the
# target's HP is below it's max HP: (only once per turn, only for first attack)
attack(15, +6, 2, 8, +4, AC = 15)

# special case for our hunter's combined attack
hunters <- function(n = 20, AC, advantage = FALSE, disadvantage = FALSE) {
  first <- attack(n, +6, 2, 8, +4, AC = AC, advantage = advantage, disadvantage = disadvantage)
  second <- attack(n, +6, 1, 8, +0, AC = AC, advantage = advantage, disadvantage = disadvantage)
  list(colossus = first, off_hand = second, sum = first$tot + second$tot)
}
#> $colossus
#> $colossus$d20
#>  [1] 12 14 11 10 13  6 19 11 16  7  2 12 14 16  5

#> $colossus$hit
#>  [1] 18 20 17 16 19 12 25 17 22 13  8 18 20 22 11

#> $colossus$crit_n
#> [1] 0

#> $colossus$crit_miss_n
#> [1] 0

#> $colossus$hit_n
#> [1] 12

#> $colossus$dmgn
#> [1] 12

#> $colossus$dmg
#>  [1] 16 11 16 13 17  8 15 18  7 12 13 15

#> $colossus$tot
#> [1] 161


#> $off_hand
#> $off_hand$d20
#>  [1]  6 10 13 14 13 14 13 10  7 16 20  5  2 13 11

#> $off_hand$hit
#>  [1] 12 16 19 20 19 20 19 16 13 22 26 11  8 19 17

#> $off_hand$crit_n
#> [1] 1

#> $off_hand$crit_miss_n
#> [1] 0

#> $off_hand$hit_n
#> [1] 12

#> $off_hand$dmgn
#> [1] 13

#> $off_hand$dmg
#>  [1] 7 4 1 1 5 2 3 2 1 8 7 7 1

#> $off_hand$tot
#> [1] 49


#> $sum
#> [1] 210

# attack with 19 archers that have a +8 bonus to hit. They are also colossus
# slayers so they roll 2d8+4 damage against an AC of 15, with advantage
attack(19, +8, 2, 8, +4, AC = 15, adv = TRUE)
#> Rolled a: 4 and a 15
#> Rolled a: 10 and a 18
#> Rolled a: 20 and a 16
#> Rolled a: 20 and a 12
#> Rolled a: 4 and a 13
#> Rolled a: 9 and a 12
#> Rolled a: 8 and a 16
#> Rolled a: 11 and a 16
#> Rolled a: 17 and a 18
#> Rolled a: 20 and a 9
#> Rolled a: 18 and a 13
#> Rolled a: 15 and a 18
#> Rolled a: 5 and a 17
#> Rolled a: 3 and a 19
#> Rolled a: 4 and a 14
#> Rolled a: 15 and a 16
#> Rolled a: 2 and a 14
#> Rolled a: 15 and a 8
#> Rolled a: 12 and a 6
#> $d20
#>  [1] 15 18 20 20 13 12 16 16 18 20 18 18 17 19 14 16 14 15 12

#> $hit
#>  [1] 23 26 28 28 21 20 24 24 26 28 26 26 25 27 22 24 22 23 20

#> $crit_n
#> [1] 3

#> $crit_miss_n
#> [1] 0

#> $hit_n
#> [1] 19

#> $dmgn
#> [1] 22

#> $dmg
#>  [1] 12  9  9 10 19 13 17 13 13 15 13  9  6 15 14  9 17  8  7 15  8 10

#> $tot
#> [1] 261

# Do a stealth check for 15 human ranger hunters, who have +6 on stealth
check(15, +6, DC = 14)
#> $d20
#>  [1] 16  4  4  7  6  3  7 14  5 18  4  3  9  7 14

#> $check
#>  [1] 22 10 10 13 12  9 13 20 11 24 10  9 15 13 20

#> $success_n
#> [1] 5
