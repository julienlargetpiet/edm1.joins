![](logo.png)

# Install

-> git clone https://github.com/julienlargetpiet/edm1

-> cd edm1

edm1 > R

R > library("devtools")

R > build()

R > install()

# `any_join_datf`

any_join_datf


## Description

Allow to perform SQL joints with more features


## Usage

```r
any_join_datf(
  inpt_datf_l,
  join_type = "inner",
  join_spe = NA,
  id_v = c(),
  excl_col = c(),
  rtn_col = c(),
  d_val = NA
)
```


## Arguments

Argument      |Description
------------- |----------------
`inpt_datf_l`     |     is a list containing all the dataframe
`join_type`     |     is the joint type. Defaults to inner but can be changed to a vector containing all the dataframes you want to take their ids to don external joints.
`join_spe`     |     can be equal to a vector to do an external joints on all the dataframes. In this case, join_type should not be equal to "inner"
`id_v`     |     is a vector containing all the ids name of the dataframes. The ids names can be changed to number of their columns taking in count their position in inpt_datf_l. It means that if my id is in the third column of the second dataframe and the first dataframe have 5 columns, the column number of the ids is 5 + 3 = 8
`excl_col`     |     is a vector containing the column names to exclude, if this vector is filled so "rtn_col" should not be filled. You can also put the column number in the manner indicated for "id_v". Defaults to c()
`rtn_col`     |     is a vector containing the column names to retain, if this vector is filled so "excl_col" should not be filled. You can also put the column number in the manner indicated for "id_v". Defaults to c()
`d_val`     |     is the default val when here is no match


## Examples

```r
datf1 <- data.frame("val"=c(1, 1, 2, 4), "ids"=c("e", "a", "z", "a"),
"last"=c("oui", "oui", "non", "oui"),
"second_ids"=c(13, 11, 12, 8), "third_col"=c(4:1))

datf2 <- data.frame("val"=c(3, 7, 2, 4, 1, 2), "ids"=c("a", "z", "z", "a", "a", "a"),
"bool"=c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE),
"second_ids"=c(13, 12, 8, 34, 22, 12))

datf3 <- data.frame("val"=c(1, 9, 2, 4), "ids"=c("a", "a", "z", "a"),
"last"=c("oui", "oui", "non", "oui"),
"second_ids"=c(13, 11, 12, 8))

print(any_join_datf(inpt_datf_l=list(datf1, datf2, datf3), join_type="inner",
id_v=c("ids", "second_ids"),
excl_col=c(), rtn_col=c()))

#  ids val ids last second_ids val ids  bool second_ids val ids last second_ids
#3 z12   2   z  non         12   7   z FALSE         12   2   z  non         12

print(any_join_datf(inpt_datf_l=list(datf1, datf2, datf3), join_type="inner", id_v=c("ids"),
excl_col=c(), rtn_col=c()))

#  ids val ids last second_ids val ids  bool second_ids val ids last second_ids
#2   a   1   a  oui         11   3   a  TRUE         13   1   a  oui         13
#3   z   2   z  non         12   7   z FALSE         12   2   z  non         12
#4   a   4   a  oui          8   4   a FALSE         34   9   a  oui         11

print(any_join_datf(inpt_datf_l=list(datf1, datf2, datf3), join_type=c(1), id_v=c("ids"),
excl_col=c(), rtn_col=c()))

#  ids val ids last second_ids  val  ids  bool second_ids  val  ids last
#1   e   1   e  oui         13 <NA> <NA>  <NA>       <NA> <NA> <NA> <NA>
#2   a   1   a  oui         11    3    a  TRUE         13    1    a  oui
#3   z   2   z  non         12    7    z FALSE         12    2    z  non
#4   a   4   a  oui          8    4    a FALSE         34    9    a  oui
#  second_ids
#1       <NA>
#2         13
#3         12
#4         11

print(any_join_datf(inpt_datf_l=list(datf2, datf1, datf3), join_type=c(1, 3),
id_v=c("ids", "second_ids"),
excl_col=c(), rtn_col=c()))

#   ids  val  ids  bool second_ids  val  ids last second_ids  val  ids last
#1  a13    3    a  TRUE         13 <NA> <NA> <NA>       <NA>    1    a  oui
#2  z12    7    z FALSE         12    2    z  non         12    2    z  non
#3   z8    2    z FALSE          8 <NA> <NA> <NA>       <NA> <NA> <NA> <NA>
#4  a34    4    a FALSE         34 <NA> <NA> <NA>       <NA> <NA> <NA> <NA>
#5  a22    1    a  TRUE         22 <NA> <NA> <NA>       <NA> <NA> <NA> <NA>
#6  a12    2    a  TRUE         12 <NA> <NA> <NA>       <NA> <NA> <NA> <NA>
#7  a13 <NA> <NA>  <NA>       <NA> <NA> <NA> <NA>       <NA> <NA> <NA> <NA>
#8  a11 <NA> <NA>  <NA>       <NA>    1    a  oui         11    9    a  oui
#9  z12 <NA> <NA>  <NA>       <NA> <NA> <NA> <NA>       <NA> <NA> <NA> <NA>
#10  a8 <NA> <NA>  <NA>       <NA>    4    a  oui          8    4    a  oui
#   second_ids
#1          13
#2          12
#3        <NA>
#4        <NA>
#5        <NA>
#6        <NA>
#7        <NA>
#8          11
#9        <NA>
#10          8

print(any_join_datf(inpt_datf_l=list(datf1, datf2, datf3), join_type=c(1), id_v=c("ids"),
excl_col=c(), rtn_col=c()))

#ids val ids last second_ids  val  ids  bool second_ids  val  ids last
#1   e   1   e  oui         13 <NA> <NA>  <NA>       <NA> <NA> <NA> <NA>
#2   a   1   a  oui         11    3    a  TRUE         13    1    a  oui
#3   z   2   z  non         12    7    z FALSE         12    2    z  non
#4   a   4   a  oui          8    4    a FALSE         34    9    a  oui
#  second_ids
#1       <NA>
#2         13
#3         12
#4         11
```


# `inner_all`

inner_all


## Description

Allow to apply inner join on n dataframes, datatables, tibble


## Usage

```r
inner_all(..., keep_val = FALSE, id_v)
```


## Arguments

Argument      |Description
------------- |----------------
`...`     |     are all the dataframes etc
`keep_val`     |     is if you want to keep the id column
`id_v`     |     is the common id of all the dataframes etc


## Examples

```r
datf1 <- data.frame(
"id1"=c(1:5),
"var1"=c("oui", "oui", "oui", "non", "non")
)

datf2 <- data.frame(
"id1"=c(1, 2, 3, 7, 9),
"var1"=c("oui2", "oui2", "oui2", "non2", "non2")
)

print(inner_all(datf1, datf2, keep_val=FALSE, id_v="id1"))

id1 var1.x var1.y
1   1    oui   oui2
2   2    oui   oui2
3   3    oui   oui2
```


# `join_n_lvl`

join_n_lvl


## Description

Allow to see the progress of the multi-level joins of the different variables modalities. Here, multi-level joins is a type of join that usually needs a concatenation of two or more variables to make a key. But here, there is no need to proceed to a concatenation. See examples.


## Usage

```r
join_n_lvl(frst_datf, scd_datf, join_type = c(), lst_pair = list())
```


## Arguments

Argument      |Description
------------- |----------------
`frst_datf`     |     is the first data.frame (table)
`scd_datf`     |     is the second data.frame (table)
`join_type`     |     is a vector containing all the join type ("left", "inner", "right") for each variable
`lst_pair`     |     is a lis of vectors. The vectors refers to a multi-level join. Each vector should have a length of 1. Each vector should have a name. Its name refers to the column name of multi-level variable and its value refers to the column name of the join variable.


## Examples

```r
datf3 <- data.frame("vil"=c("one", "one", "one", "two", "two", "two"),
"charac"=c(1, 2, 2, 1, 2, 2),
"rev"=c(1250, 1430, 970, 1630, 2231, 1875),
"vil2" = c("one", "one", "one", "two", "two", "two"),
"idl2" = c(1:6))
datf4 <- data.frame("vil"=c("one", "one", "one", "two", "two", "three"),
"charac"=c(1, 2, 2, 1, 1, 2),
"rev"=c(1.250, 1430, 970, 1630, 593, 456),
"vil2" = c("one", "one", "one", "two", "two", "two"),
"idl2" = c(2, 3, 1, 5, 5, 5))

print(join_n_lvl(frst_datf=datf3, scd_datf=datf4, lst_pair=list(c("charac" = "vil"), c("vil2" = "idl2")),
join_type=c("inner", "left")))

[1] "pair: charac vil"
|  |   0%
1
|= |  50%
2
|==| 100%
[1] "pair: vil2 idl2"
|  |   0%
one
|= |  50%
two
|==| 100%

main_id.x vil.x charac.x rev.x vil2.x idl2.x main_id.y vil.y charac.y rev.y
1  1oneone1   one        1  1250    one      1      <NA>  <NA>       NA    NA
2  2oneone2   one        2  1430    one      2      <NA>  <NA>       NA    NA
3  2oneone3   one        2   970    one      3  2oneone3   one        2  1430
4  1twotwo4   two        1  1630    two      4      <NA>  <NA>       NA    NA
vil2.y idl2.y
1   <NA>     NA
2   <NA>     NA
3    one      3
4   <NA>     NA
```


# `left_all`

left_all


## Description

Allow to apply left join on n dataframes, datatables, tibble


## Usage

```r
left_all(..., keep_val = FALSE, id_v)
```


## Arguments

Argument      |Description
------------- |----------------
`...`     |     are all the dataframes etc
`keep_val`     |     is if you want to keep the id column
`id_v`     |     is the common id of all the dataframes etc


## Examples

```r
datf1 <- data.frame(
"id1"=c(1:5),
"var1"=c("oui", "oui", "oui", "non", "non")
)

datf2 <- data.frame(
"id1"=c(1, 2, 3, 7, 9),
"var1"=c("oui2", "oui2", "oui2", "non2", "non2")
)

print(left_all(datf1, datf2, datf2, datf2, keep_val=FALSE, id_v="id1"))

id1 var1.x var1.y var1.x.x var1.y.y
1   1    oui   oui2     oui2     oui2
2   2    oui   oui2     oui2     oui2
3   3    oui   oui2     oui2     oui2
4   4    non   <NA>     <NA>     <NA>
5   5    non   <NA>     <NA>     <NA>#'
print(left_all(datf1, datf2, datf2, keep_val=FALSE, id_v="id1"))

id1 var1.x var1.y var1
1   1    oui   oui2 oui2
2   2    oui   oui2 oui2
3   3    oui   oui2 oui2
4   4    non   <NA> <NA>
5   5    non   <NA> <NA>
```


