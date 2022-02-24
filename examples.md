### some examples

Load the functions so we can call them

``` r
source("roll_dnd_mobs.R")
```

Attack with 15 level 3 human ranger hunters with a shortsword.

``` r
attack(24, +6, 1, 8, +4, AC = 15)
```

    ## $d20
    ##  [1]  8  6 20  5 11 15 17 12  2 19 16 16 13  3 10  1 19 14  3  4 18  7 19 19
    ## 
    ## $hit
    ##  [1] 14 12 26 11 17 21 23 18  8 25 22 22 19  9 16  7 25 20  9 10 24 13 25 25
    ## 
    ## $crit_n
    ## [1] 1
    ## 
    ## $crit_miss_n
    ## [1] 1
    ## 
    ## $hit_n
    ## [1] 15
    ## 
    ## $dmgn
    ## [1] 16
    ## 
    ## $dmg
    ##  [1]  6 12  6  6  5 12  8  9 10  8 12 10  8  5 12  6
    ## 
    ## $tot
    ## [1] 135

They can also attack with their off-hand using their bonus action, but
for the damage we do not add the ability modifier.

``` r
attack(24, +6, 1, 8, +0, AC = )
```

    ## $d20
    ##  [1] 10 15  7 13 17 19  8 17 16  6 11 19  7  6  6 13 12 16 16 13  8 11  9  7
    ## 
    ## $hit
    ##  [1] 16 21 13 19 23 25 14 23 22 12 17 25 13 12 12 19 18 22 22 19 14 17 15 13
    ## 
    ## $crit_n
    ## [1] 0
    ## 
    ## $crit_miss_n
    ## [1] 0
    ## 
    ## $hit_n
    ## [1] 16
    ## 
    ## $dmgn
    ## [1] 16
    ## 
    ## $dmg
    ##  [1] 7 2 5 6 4 6 1 2 4 6 8 5 1 4 6 5
    ## 
    ## $tot
    ## [1] 72

## monsters

the undead armies vs. dragon

``` r
attack(29, +4, 1, 6, +2, AC = 18)
```

    ## $d20
    ##  [1] 14  8 18  5  8  2 17  4 11  1 14 17  1  9  7  3  1 16 19 19 14  4 19 20 11
    ## [26] 10 20 15  8
    ## 
    ## $hit
    ##  [1] 18 12 22  9 12  6 21  8 15  5 18 21  5 13 11  7  5 20 23 23 18  8 23 24 15
    ## [26] 14 24 19 12
    ## 
    ## $crit_n
    ## [1] 2
    ## 
    ## $crit_miss_n
    ## [1] 3
    ## 
    ## $hit_n
    ## [1] 13
    ## 
    ## $dmgn
    ## [1] 15
    ## 
    ## $dmg
    ##  [1] 5 6 4 8 3 5 5 5 6 4 3 8 5 5 3
    ## 
    ## $tot
    ## [1] 75

de frost giants

``` r
attack(6, +4, 3, 12, +4, AC = 17)
```

    ## $d20
    ## [1]  8 12 13 20 10 14
    ## 
    ## $hit
    ## [1] 12 16 17 24 14 18
    ## 
    ## $crit_n
    ## [1] 1
    ## 
    ## $crit_miss_n
    ## [1] 0
    ## 
    ## $hit_n
    ## [1] 3
    ## 
    ## $dmgn
    ## [1] 4
    ## 
    ## $dmg
    ## [1] 35 27 16 13
    ## 
    ## $tot
    ## [1] 91

Our hunters are colossus slayers, so they deal an extra 1d8 damage if
the target’s HP is below it’s max HP: (only once per turn, only for
first attack).

``` r
attack(15, +6, 2, 8, +4, AC = 15)
```

    ## $d20
    ##  [1]  8 14 17  9  7 16 19  9  7 18 19 16 11  9  5
    ## 
    ## $hit
    ##  [1] 14 20 23 15 13 22 25 15 13 24 25 22 17 15 11
    ## 
    ## $crit_n
    ## [1] 0
    ## 
    ## $crit_miss_n
    ## [1] 0
    ## 
    ## $hit_n
    ## [1] 11
    ## 
    ## $dmgn
    ## [1] 11
    ## 
    ## $dmg
    ##  [1] 11 13  9 12 12 12  9  8  9 14 10
    ## 
    ## $tot
    ## [1] 119

Define a special case for our hunter’s combined attack (you can too!)

``` r
hunters <- function(n = 20, AC, advantage = FALSE, disadvantage = FALSE) {
  first <- attack(n, +6, 2, 8, +4, AC = AC, advantage = advantage, disadvantage = disadvantage)
  second <- attack(n, +6, 1, 8, +0, AC = AC, advantage = advantage, disadvantage = disadvantage)
  list(colossus = first, off_hand = second, sum = first$tot + second$tot)
}
```

Attack with 19 archers that have a +8 bonus to hit. They are also
colossus slayers so they roll 2d8+4 damage against an AC of 15, with
advantage.

``` r
attack(n_mobs = 20, to_hit = +8, dmg_die_n = 2, dmg_die = 8, dmg_bonus = +4, AC = 15)
```

    ## $d20
    ##  [1] 17 10 17  9 13  5  9 18 11 17  8 17  6  2 16  7  5  6  5 11
    ## 
    ## $hit
    ##  [1] 25 18 25 17 21 13 17 26 19 25 16 25 14 10 24 15 13 14 13 19
    ## 
    ## $crit_n
    ## [1] 0
    ## 
    ## $crit_miss_n
    ## [1] 0
    ## 
    ## $hit_n
    ## [1] 14
    ## 
    ## $dmgn
    ## [1] 14
    ## 
    ## $dmg
    ##  [1] 11 13 19 14 18 17 11 17 18 10 15 16 10 14
    ## 
    ## $tot
    ## [1] 203

Do a stealth check for 15 human ranger hunters, who have +6 on stealth.

``` r
check(15, +6, DC = 14)
```

    ## $d20
    ##  [1]  3 16 14  8 15  9 17 14 17 17 17  2  2  3  4
    ## 
    ## $check
    ##  [1]  9 22 20 14 21 15 23 20 23 23 23  8  8  9 10
    ## 
    ## $success_n
    ## [1] 10

A saving throw is the same as a skill check.

``` r
save(15, +6, DC = 14)
```

    ## $d20
    ##  [1]  8  2  7  8 17  3 13  9 20 12 20 13  1  6  1
    ## 
    ## $check
    ##  [1] 14  8 13 14 23  9 19 15 26 18 26 19  7 12  7
    ## 
    ## $success_n
    ## [1] 9
