#' any_join_datf
#'
#' Allow to perform SQL joints with more features
#' @param inpt_datf_l is a list containing all the dataframe
#' @param join_type is the joint type. Defaults to inner but can be changed to a vector containing all the dataframes you want to take their ids to don external joints.
#' @param join_spe can be equal to a vector to do an external joints on all the dataframes. In this case, join_type should not be equal to "inner"
#' @param id_v is a vector containing all the ids name of the dataframes. The ids names can be changed to number of their columns taking in count their position in inpt_datf_l. It means that if my id is in the third column of the second dataframe and the first dataframe have 5 columns, the column number of the ids is 5 + 3 = 8
#' @param excl_col is a vector containing the column names to exclude, if this vector is filled so "rtn_col" should not be filled. You can also put the column number in the manner indicated for "id_v". Defaults to c()
#' @param rtn_col is a vector containing the column names to retain, if this vector is filled so "excl_col" should not be filled. You can also put the column number in the manner indicated for "id_v". Defaults to c()
#' @param d_val is the default val when here is no match 
#' @examples
#'
#'datf1 <- data.frame("val"=c(1, 1, 2, 4), "ids"=c("e", "a", "z", "a"), 
#'"last"=c("oui", "oui", "non", "oui"),
#'"second_ids"=c(13, 11, 12, 8), "third_col"=c(4:1))
#'
#'datf2 <- data.frame("val"=c(3, 7, 2, 4, 1, 2), "ids"=c("a", "z", "z", "a", "a", "a"), 
#'"bool"=c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE),
#'"second_ids"=c(13, 12, 8, 34, 22, 12))
#'
#'datf3 <- data.frame("val"=c(1, 9, 2, 4), "ids"=c("a", "a", "z", "a"), 
#'"last"=c("oui", "oui", "non", "oui"),
#'"second_ids"=c(13, 11, 12, 8))
#'
#'print(any_join_datf(inpt_datf_l=list(datf1, datf2, datf3), join_type="inner", 
#'id_v=c("ids", "second_ids"), 
#'                  excl_col=c(), rtn_col=c()))
#' 
#'#  ids val ids last second_ids val ids  bool second_ids val ids last second_ids
#'#3 z12   2   z  non         12   7   z FALSE         12   2   z  non         12
#'
#'print(any_join_datf(inpt_datf_l=list(datf1, datf2, datf3), join_type="inner", id_v=c("ids"),
#'excl_col=c(), rtn_col=c()))
#'
#'#  ids val ids last second_ids val ids  bool second_ids val ids last second_ids
#'#2   a   1   a  oui         11   3   a  TRUE         13   1   a  oui         13
#'#3   z   2   z  non         12   7   z FALSE         12   2   z  non         12
#'#4   a   4   a  oui          8   4   a FALSE         34   9   a  oui         11
#'
#'print(any_join_datf(inpt_datf_l=list(datf1, datf2, datf3), join_type=c(1), id_v=c("ids"), 
#'                  excl_col=c(), rtn_col=c()))
#'
#'#  ids val ids last second_ids  val  ids  bool second_ids  val  ids last
#'#1   e   1   e  oui         13 <NA> <NA>  <NA>       <NA> <NA> <NA> <NA>
#'#2   a   1   a  oui         11    3    a  TRUE         13    1    a  oui
#'#3   z   2   z  non         12    7    z FALSE         12    2    z  non
#'#4   a   4   a  oui          8    4    a FALSE         34    9    a  oui
#'#  second_ids
#'#1       <NA>
#'#2         13
#'#3         12
#'#4         11
#'
#'print(any_join_datf(inpt_datf_l=list(datf2, datf1, datf3), join_type=c(1, 3), 
#'                  id_v=c("ids", "second_ids"), 
#'                  excl_col=c(), rtn_col=c()))
#' 
#'#   ids  val  ids  bool second_ids  val  ids last second_ids  val  ids last
#'#1  a13    3    a  TRUE         13 <NA> <NA> <NA>       <NA>    1    a  oui
#'#2  z12    7    z FALSE         12    2    z  non         12    2    z  non
#'#3   z8    2    z FALSE          8 <NA> <NA> <NA>       <NA> <NA> <NA> <NA>
#'#4  a34    4    a FALSE         34 <NA> <NA> <NA>       <NA> <NA> <NA> <NA>
#'#5  a22    1    a  TRUE         22 <NA> <NA> <NA>       <NA> <NA> <NA> <NA>
#'#6  a12    2    a  TRUE         12 <NA> <NA> <NA>       <NA> <NA> <NA> <NA>
#'#7  a13 <NA> <NA>  <NA>       <NA> <NA> <NA> <NA>       <NA> <NA> <NA> <NA>
#'#8  a11 <NA> <NA>  <NA>       <NA>    1    a  oui         11    9    a  oui
#'#9  z12 <NA> <NA>  <NA>       <NA> <NA> <NA> <NA>       <NA> <NA> <NA> <NA>
#'#10  a8 <NA> <NA>  <NA>       <NA>    4    a  oui          8    4    a  oui
#'#   second_ids
#'#1          13
#'#2          12
#'#3        <NA>
#'#4        <NA>
#'#5        <NA>
#'#6        <NA>
#'#7        <NA>
#'#8          11
#'#9        <NA>
#'#10          8
#'
#'print(any_join_datf(inpt_datf_l=list(datf1, datf2, datf3), join_type=c(1), id_v=c("ids"), 
#'                  excl_col=c(), rtn_col=c()))
#'
#'#ids val ids last second_ids  val  ids  bool second_ids  val  ids last
#'#1   e   1   e  oui         13 <NA> <NA>  <NA>       <NA> <NA> <NA> <NA>
#'#2   a   1   a  oui         11    3    a  TRUE         13    1    a  oui
#'#3   z   2   z  non         12    7    z FALSE         12    2    z  non
#'#4   a   4   a  oui          8    4    a FALSE         34    9    a  oui
#'#  second_ids
#'#1       <NA>
#'#2         13
#'#3         12
#'#4         11
#' 
#' @export

any_join_datf <- function(inpt_datf_l, join_type="inner", join_spe=NA, id_v=c(),  
                    excl_col=c(), rtn_col=c(), d_val=NA){

    incr_fillr <- function(inpt_v, wrk_v=NA, default_val=NA, step=1){

            if (all(is.na(wrk_v))){

                rtn_v <- inpt_v

            }else{

                rtn_v <- wrk_v

            }

            if (is.na(default_val)){

                i = 2

                while (i <= length(inpt_v)){

                    if (is.na(inpt_v[(i-1)]) == FALSE){

                        if ((inpt_v[(i-1)] + step) < inpt_v[i]){

                            rtn_v <- append(x=rtn_v, values=default_val, after=(i-1))

                            inpt_v <- append(x=inpt_v, values=default_val, after=(i-1))

                            bf_val = inpt_v[(i-1)] + 1

                        }

                    }else if ((bf_val + step) < inpt_v[i]){

                            rtn_v <- append(x=rtn_v, values=default_val, after=(i-1))

                            inpt_v <- append(x=inpt_v, values=default_val, after=(i-1))

                            bf_val = bf_val + 1

                    }

                    i = i + 1

                }

            }else if (default_val != "increasing"){

                i = 2

                while (i <= length(inpt_v)){

                    if (inpt_v[(i-1)] != default_val){

                        if ((as.numeric(inpt_v[(i-1)]) + step) < as.numeric(inpt_v[i])){

                            rtn_v <- append(x=rtn_v, values=default_val, after=(i-1))

                            inpt_v <- append(x=inpt_v, values=default_val, after=(i-1))

                            bf_val = as.numeric(inpt_v[(i-1)]) + 1

                        }

                    }else if ((bf_val + step) < as.numeric(inpt_v[i])){

                            inpt_v <- append(x=inpt_v, values=default_val, after=(i-1))

                            rtn_v <- append(x=rtn_v, values=default_val, after=(i-1))

                            bf_val = bf_val + 1

                    }

                    i = i + 1

                }

            }else{

                i = 2

                while (i <= length(rtn_v)){

                    if ((inpt_v[(i-1)] + step) < inpt_v[i]){

                        rtn_v <- append(x=rtn_v, values=(inpt_v[(i-1)]+1), after=(i-1))

                        inpt_v <- append(x=inpt_v, values=(inpt_v[(i-1)]+1), after=(i-1))

                    }

                    i = i + 1

                }

            }

            return(rtn_v)

    }

    fixer_nest_v <- function(cur_v, pttrn_v, wrk_v){

            cnt = 1

            cnt2 = 0

            for (i in 1:length(cur_v)){

                if (pttrn_v[cnt] != cur_v[i]){

                    if (cnt2 == 0){

                        idx <- (cnt2*length(pttrn_v)-1) + match(TRUE, str_detect(pttrn_v, paste0("\\b(", cur_v[i], ")\\b"))) 

                    }else{

                        idx <- cnt2*length(pttrn_v) + match(TRUE, str_detect(pttrn_v, paste0("\\b(", cur_v[i], ")\\b"))) 

                    }

                    rtain_val <- wrk_v[idx]

                    wrk_v[idx] <- wrk_v[i] 

                    wrk_v[i] <- rtain_val

                    rtain_val <- cur_v[idx]

                    cur_v[idx] <- cur_v[i]

                    cur_v[i] <- rtain_val

                    if (cnt == length(pttrn_v)){ cnt = 1; cnt2 = cnt2 + 1 }else if (cnt > 1) { cnt = cnt + 1 }

                }else{

                    if (cnt == length(pttrn_v)){ cnt = 1; cnt2 = cnt2 + 1 }else { cnt = cnt + 1 }

                }

            }

            return(wrk_v)

        }

    appndr <- function(inpt_v, val=NA, hmn, strt="max"){

        if (strt == "max"){

            strt <- length(inpt_v)

        }

        if (hmn > 0){

            for (i in hmn){ inpt_v <- append(x=inpt_v, values=val, after=strt) }

        }

        return(inpt_v)

    }

    calc_occu_v <- function(f_v, w_v, nvr_here=NA){

            rtn_v <- c()

            idx_status <- c()

            f_v2 <- f_v

            for (el in 1:length(w_v)){

                cur_ids <- match(w_v[el], f_v)

                f_v[cur_ids] <- nvr_here

                idx_status <- c(idx_status, cur_ids)

            }

            for (i in sort(idx_status)){

                idx <- match(f_v2[i], w_v)

                rtn_v <- c(rtn_v, idx)

                w_v[idx] <- nvr_here

            }

            return(rtn_v)

    }

    extrt_only_v <- function(inpt_v, pttrn_v){

            rtn_v <- c()

            for (el in inpt_v){

                if (el %in% pttrn_v){ rtn_v <- c(rtn_v, el) }

            }

            return(rtn_v)

    }

    nest_v <- function(f_v, t_v, step=1, after=1){

        cnt = after

        for (i in 1:length(t_v)){

            f_v <- append(x=f_v, values=t_v[i], after=cnt)

            cnt = cnt + step + 1

        }

        return(f_v)

    }

    paste_datf <- function(inpt_datf, sep=""){

        if (ncol(as.data.frame(inpt_datf)) == 1){ 

            return(inpt_datf) 

        }else {

            rtn_datf <- inpt_datf[,1]

            for (i in 2:ncol(inpt_datf)){

                rtn_datf <- paste(rtn_datf, inpt_datf[,i], sep=sep)

            }

            return(rtn_datf)

        }

    }

    n_row <- 1

    col_intel <- c()

    for (datf_ in inpt_datf_l){ 

        if (nrow(datf_) > n_row){ n_row <- nrow(datf_) }

        col_intel <- c(col_intel, (sum(col_intel) + ncol(datf_)))

    }

    cl_nms <- colnames(as.data.frame(inpt_datf_l[1]))

    if (length(inpt_datf_l) > 1){

            for (i in 2:length(inpt_datf_l)){

                cl_nms <- c(cl_nms, colnames(as.data.frame(inpt_datf_l[i])))

            }

    }

    if (length(excl_col) > 0 & length(rtn_col) == 0){

            pre_col <- c(1:sum(mapply(function(x) return(ncol(x)), inpt_datf_l)))

            if (typeof(excl_col) == "character"){

                excl_col2 <- c() 

                for (el in excl_col){ excl_col2 <- c(excl_col2, match(el, cl_nms)) }

                pre_col <- pre_col[-excl_col2]

            }else{

                pre_col <- pre_col[-excl_col]

            }

    }else if ((length(excl_col) + length(rtn_col)) == 0){

        pre_col <- c(1:sum(mapply(function(x) return(ncol(x)), inpt_datf_l)))

    }else{

        if (typeof(rtn_col) == "character"){

            pre_col <- c()

            for (el in rtn_col){ pre_col <- c(pre_col, match(el, cl_nms)) }

        }else{

            pre_col <- rtn_col

        }

    }

    if (typeof(id_v) == "character"){

        id_v2 <- which(cl_nms == id_v[1])

        if (length(id_v) > 1){

            for (i in 2:length(id_v)){ id_v2 <- nest_v(f_v=id_v2, t_v=which(cl_nms == id_v[i]), after=(i-1)) }

        }

        id_v2 <- fixer_nest_v(cur_v=extrt_only_v(inpt_v=cl_nms, pttrn_v=id_v), pttrn_v=id_v, wrk_v=id_v2)

    }

    col_intel_cnt = 1 

    id_v_cnt = 1

    pre_col <- sort(pre_col)

    substrct <- 0

    ids_val_func <- function(x){

            lst_el <- length(which(lst_ids == x))

            if (length(which(cur_ids == x)) > lst_el){ 

                    return(which(cur_ids == x)[1:lst_el]) 

            }else {

                    return(which(cur_ids == x)[1:length(which(cur_ids == x))])

            }

    }

    if (all(join_type == "inner") & all(is.na(join_spe))){

        cur_datf <- as.data.frame(inpt_datf_l[1])

        cur_id_v <- id_v2[1:length(id_v)]

        rtn_datf <- cur_datf[, cur_id_v]

        cur_ids <- paste_datf(cur_datf[, cur_id_v])

        rtn_datf <- data.frame(cur_ids)

        cur_ids_val <- c(1:nrow(cur_datf))

        calc_ids <- c(1:nrow(rtn_datf))

        for (cur_col in pre_col){

            while (col_intel[col_intel_cnt] < cur_col){

                lst_ids <- cur_ids[cur_ids_val]

                id_v_cnt = id_v_cnt + length(id_v)

                col_intel_cnt = col_intel_cnt + 1

                cur_datf <- as.data.frame(inpt_datf_l[col_intel_cnt])

                cur_id_v <- id_v2[id_v_cnt:(id_v_cnt+length(id_v)-1)] 

                cur_ids <- paste_datf(cur_datf[, 
                    cur_id_v-(sum(mapply(function(x) return(ncol(x)), inpt_datf_l[1:(col_intel_cnt-1)])))])

                cur_ids_val2 <- sort(lst_flatnr(mapply(function(x) return(which(lst_ids == x)), unique(cur_ids))))

                rtn_datf <- rtn_datf[cur_ids_val2, ]

                cur_ids_val <- sort(lst_flatnr(mapply(function(x) return(ids_val_func(x)), unique(lst_ids[cur_ids_val2]))))

                substrct <- sum(mapply(function(x) return(ncol(x)), inpt_datf_l[1:(col_intel_cnt-1)]))

                calc_ids <- calc_occu_v(f_v=lst_ids, w_v=cur_ids[cur_ids_val])

                calc_ids <- calc_ids[is.na(calc_ids)==FALSE]

            }

            pre_rtn_datf <- cur_datf[cur_ids_val, (cur_col - substrct)]

            pre_rtn_datf <- pre_rtn_datf[calc_ids]

            rtn_datf <- cbind(rtn_datf, pre_rtn_datf)

            colnames(rtn_datf)[length(colnames(rtn_datf))] <- cl_nms[cur_col]

        }

        colnames(rtn_datf)[1] <- "ids"

        return(rtn_datf)

    }else{

        spe_match <- function(f_v, w_v, nvr_here=NA){

            rtn_v <- c()

            for (i in 1:length(w_v)){

                idx <- match(w_v[i], f_v)

                rtn_v <- c(rtn_v, idx)

                f_v[idx] <- nvr_here

            }

            return(rtn_v)

        }

        if (is.na(join_spe)){

                strt_id <- 1

                cur_datf <- as.data.frame(inpt_datf_l[join_type[1]])

                cur_id_v <- id_v2[strt_id:length(id_v)]

                cur_ids <- paste_datf(cur_datf[, cur_id_v])

                if (length(join_type) > 1){

                        join_type <- join_type[2:length(join_type)]

                        for (datf in join_type){

                                    strt_id <- length(id_v) * (datf-1) + 1

                                    cur_datf <- as.data.frame(inpt_datf_l[datf])

                                    cur_id_v <- id_v2[strt_id:(strt_id+length(id_v)-1)]

                                    cur_ids <- c(cur_ids, paste_datf(cur_datf[, 
                                cur_id_v - sum(mapply(function(x) return(ncol(x)), inpt_datf_l[1:(datf-1)]))]))

                        }

                        cur_datf <- as.data.frame(inpt_datf_l[1])

                }

                lst_ids <- cur_ids

                cur_ids_val <- sort(lst_flatnr(mapply(function(x) return(which(lst_ids == x)), unique(cur_ids))))

        }else{

                lst_ids <- cur_ids

                cur_datf <- as.data.frame(inpt_datf_l[1])

                cur_id_v <- id_v2[1:length(id_v)]

                cur_ids <- paste_datf(cur_datf[, cur_id_v])

                cur_ids_val <- sort(lst_flatnr(mapply(function(x) return((which(lst_ids == x))), unique(cur_ids))))

        }

        rtn_datf <- data.frame(cur_ids)

        cur_ids_val2 <- c(1:nrow(rtn_datf)) 

        calc_ids <- c(1:length(lst_ids))

        for (cur_col in pre_col){

            while (col_intel[col_intel_cnt] < cur_col){

                col_intel_cnt = col_intel_cnt + 1

                substrct <- sum(mapply(function(x) return(ncol(x)), inpt_datf_l[1:(col_intel_cnt-1)]))

                cur_datf <- as.data.frame(inpt_datf_l[col_intel_cnt])

                id_v_cnt = id_v_cnt + length(id_v)

                cur_id_v <- id_v2[id_v_cnt:(id_v_cnt+(length(id_v)-1))]

                cur_ids <- paste_datf(cur_datf[, (cur_id_v - substrct)])

                cur_ids_val2 <- lst_flatnr(mapply(function(x) return(ids_val_func(x)), unique(lst_ids)))

                cur_ids_val2 <- cur_ids_val2[is.na(cur_ids_val2)==FALSE]

                cur_ids_val <- sort(spe_match(f_v=lst_ids, w_v=cur_ids[cur_ids_val2]))

                cur_ids_val <- c(0, cur_ids_val)

                calc_ids <- calc_occu_v(f_v=lst_ids, w_v=cur_ids[cur_ids_val2])

                calc_ids <- calc_ids[(is.na(calc_ids)==F)]

            }

            pre_rtn_datf <- cur_datf[cur_ids_val2, 
                (cur_col - substrct)]

            pre_rtn_datf <- pre_rtn_datf[calc_ids]

            pre_rtn_datf <- incr_fillr(inpt_v=unique(c(cur_ids_val, length(lst_ids))), wrk_v=c("NA", pre_rtn_datf), 
                                     default_val=d_val)

            pre_rtn_datf <- appndr(inpt_v=pre_rtn_datf, val=d_val, hmn=(length(lst_ids) - (length(pre_rtn_datf) - 1)), strt="max")

            rtn_datf <- cbind(rtn_datf, pre_rtn_datf[2:length(pre_rtn_datf)])

            colnames(rtn_datf)[length(colnames(rtn_datf))] <- cl_nms[cur_col]

        }

        colnames(rtn_datf)[1] <- "ids"

        return(rtn_datf)
        
    }

}

#' join_n_lvl
#'
#' Allow to see the progress of the multi-level joins of the different variables modalities. Here, multi-level joins is a type of join that usually needs a concatenation of two or more variables to make a key. But here, there is no need to proceed to a concatenation. See examples. 
#'
#' @param frst_datf is the first data.frame (table)
#' @param scd_datf is the second data.frame (table)
#' @param lst_pair is a lis of vectors. The vectors refers to a multi-level join. Each vector should have a length of 1. Each vector should have a name. Its name refers to the column name of multi-level variable and its value refers to the column name of the join variable. 
#' @param join_type is a vector containing all the join type ("left", "inner", "right") for each variable
#' @examples
#' 
#' datf3 <- data.frame("vil"=c("one", "one", "one", "two", "two", "two"),
#'                      "charac"=c(1, 2, 2, 1, 2, 2),
#'                      "rev"=c(1250, 1430, 970, 1630, 2231, 1875),
#'                      "vil2" = c("one", "one", "one", "two", "two", "two"),
#'                      "idl2" = c(1:6))
#' datf4 <- data.frame("vil"=c("one", "one", "one", "two", "two", "three"),
#'                     "charac"=c(1, 2, 2, 1, 1, 2),
#'                      "rev"=c(1.250, 1430, 970, 1630, 593, 456),
#'                      "vil2" = c("one", "one", "one", "two", "two", "two"),
#'                      "idl2" = c(2, 3, 1, 5, 5, 5))
#' 
#' print(join_n_lvl(frst_datf=datf3, scd_datf=datf4, lst_pair=list(c("charac" = "vil"), c("vil2" = "idl2")), 
#'                  join_type=c("inner", "left")))
#'
#' [1] "pair: charac vil"
#' |  |   0%
#' 1 
#' |= |  50%
#' 2 
#' |==| 100%
#' [1] "pair: vil2 idl2"
#' |  |   0%
#' one 
#' |= |  50%
#' two 
#' |==| 100%
#' 
#'   main_id.x vil.x charac.x rev.x vil2.x idl2.x main_id.y vil.y charac.y rev.y
#' 1  1oneone1   one        1  1250    one      1      <NA>  <NA>       NA    NA
#' 2  2oneone2   one        2  1430    one      2      <NA>  <NA>       NA    NA
#' 3  2oneone3   one        2   970    one      3  2oneone3   one        2  1430
#' 4  1twotwo4   two        1  1630    two      4      <NA>  <NA>       NA    NA
#'   vil2.y idl2.y
#' 1   <NA>     NA
#' 2   <NA>     NA
#' 3    one      3
#' 4   <NA>     NA
#' 
#' @export

join_n_lvl <- function(frst_datf, scd_datf, join_type=c(),
                       lst_pair=list()){
  better_match <- function(inpt_v=c(), ptrn, untl=1, nvr_here=NA){
    Rtn_v <- c()
    for (cur_ptrn in ptrn){
      rtn_v <- c()
      cnt = 1
      stop <- FALSE
      while (length(rtn_v) < untl & cnt < (length(inpt_v) + 1) & !(stop)){
              pre_match <- match(x=cur_ptrn, table=inpt_v)
              if (!(is.na(pre_match))){
                inpt_v[pre_match] <- nvr_here
                rtn_v <- c(rtn_v, pre_match)
              }else{
                stop <- TRUE
              }
              cnt = cnt + 1
      }
      Rtn_v <- c(Rtn_v, rtn_v)
    }
    return(Rtn_v)
  }
  if (length(lst_pair) > 0){
    if (length(join_type) < length(lst_pair)){
      val_add <- join_type[length(join_type)]
      while (length(join_type) < (length(lst_pair) - 1)){
        join_type <- c(join_type, val_add)  
      }
    }
    frst_datf <- cbind(data.frame("main_id" = matrix(data="", 
                        nrow=nrow(frst_datf), ncol=1)), frst_datf)
    scd_datf <- cbind(data.frame("main_id" = matrix(data="", 
                        nrow=nrow(scd_datf), ncol=1)), scd_datf)
    colnames(frst_datf) <- paste0(colnames(frst_datf), ".x")
    colnames(scd_datf) <- paste0(colnames(scd_datf), ".y")
    stay_col <- colnames(frst_datf)
    cur_vec <- c()
    cur_datf <- NULL
    for (cl in 1:length(lst_pair)){
      frst_datf2 <- data.frame(matrix(data=NA, nrow=0, 
                      ncol=(ncol(scd_datf)+ncol(frst_datf))))
      cur_by <- unlist(lst_pair[cl])
      print(paste("pair:", names(cur_by), cur_by[1]))
      cur_col <- match(x=paste0(names(cur_by), ".x"), table=stay_col)
      cur_coly <- match(x=paste0(names(cur_by), ".y"), table=colnames(scd_datf))
      if (!(is.null(cur_datf))){
        scd_datf <- frst_datf[, (length(stay_col) + 1):ncol(frst_datf)]
        frst_datf <- frst_datf[, 1:length(stay_col)]
      }
      frst_datf$main_id.x <- paste0(frst_datf$main_id.x, frst_datf[, 
                        match(x=paste0(names(cur_by), ".x"), table=stay_col)])
      frst_datf$main_id.x <- paste0(frst_datf$main_id.x, frst_datf[, 
                        match(x=paste0(cur_by[1], ".x"), table=stay_col)])
      scd_datf$main_id.y <- paste0(scd_datf$main_id.y, scd_datf[, 
                                match(x=paste0(names(cur_by), ".y"), table=colnames(scd_datf))])
      scd_datf$main_id.y <- paste0(scd_datf$main_id.y, scd_datf[, 
                                match(x=paste0(cur_by[1], ".y"), table=colnames(scd_datf))])
      if (join_type[cl] == "left"){
        keep_r <- better_match(ptrn = unique(frst_datf[, cur_col]), 
                         inpt_v = scd_datf[, cur_coly], untl = nrow(scd_datf))
        scd_datf <- scd_datf[keep_r, ]
        uncf <- unique(frst_datf[, cur_col])
        pb <- txtProgressBar(min = 0,
                     max = length(uncf),
                     style = 3,
                     width = length(uncf), 
                     char = "=")
        cat("\n")
        for (rws in 1:length(uncf)){
          cur_datf <- left_join(
            x = frst_datf[frst_datf[, cur_col] == uncf[rws], ], 
            y = scd_datf[scd_datf[, cur_coly] == uncf[rws], ],
            keep = TRUE,
            by = c("main_id.x" = "main_id.y"),
            multiple = "first"
          )
          cat(uncf[rws], "\n")
          setTxtProgressBar(pb, rws)
          cat("\n")
          frst_datf2 <- rbind(frst_datf2, cur_datf)
        }
        frst_datf <- frst_datf2
      }else if (join_type[cl] == "inner"){
        keep_r <- better_match(ptrn = unique(frst_datf[, cur_col]), 
                         inpt_v = scd_datf[, cur_coly], untl = nrow(scd_datf))
        scd_datf <- scd_datf[keep_r, ]        
        uncf <- unique(frst_datf[, cur_col])
        pb <- txtProgressBar(min = 0,
                     max = length(uncf),
                     style = 3,
                     width = length(uncf), 
                     char = "=")
        cat("\n")
        for (rws in 1:length(uncf)){
          cur_datf <- inner_join(
            x = frst_datf[frst_datf[, cur_col] == uncf[rws], ], 
            y = scd_datf[scd_datf[, cur_coly] == uncf[rws], ],
            keep = TRUE,
            by = c("main_id.x" = "main_id.y"),
            multiple = "first"
          )
          cat(uncf[rws], "\n")
          setTxtProgressBar(pb, rws)
          cat("\n")
          frst_datf2 <- rbind(frst_datf2, cur_datf)
        }
        frst_datf <- frst_datf2
      }else if (join_type[cl] == "right"){
        keep_r <- better_match(ptrn = unique(scd_datf[, cur_coly]), 
                         inpt_v = frst_datf[, cur_col], untl = nrow(scd_datf))
        frst_datf <- frst_datf[keep_r, ]
        uncf <- unique(scd_datf[, cur_coly])
        pb <- txtProgressBar(min = 0,
                     max = length(uncf),
                     style = 3,
                     width = length(uncf), 
                     char = "=")
        cat("\n")
        for (rws in 1:length(uncf)){
          cur_datf <- right_join(
            x = frst_datf[frst_datf[, cur_col] == uncf[rws], ], 
            y = scd_datf[scd_datf[, cur_coly] == uncf[rws], -cur_coly],
            keep = TRUE,
            by = c("main_id.x" = "main_id.y"),
            multiple = "first"
          )
          cat(uncf[rws], "\n")
          setTxtProgressBar(pb, rws)
          cat("\n")
          frst_datf2 <- rbind(frst_datf2, cur_datf)
        }
        frst_datf <- frst_datf2
      }else {
        return(NULL)
      }
    }
    close(pb)
    return(frst_datf2)
  }else{
    return(NULL)
  }
}

#' left_all
#'
#' Allow to apply left join on n dataframes, datatables, tibble
#'
#' @param ... are all the dataframes etc
#' @param keep_val is if you want to keep the id column
#' @param id_v is the common id of all the dataframes etc
#' @examples
#'
#' datf1 <- data.frame(
#'         "id1"=c(1:5),
#'         "var1"=c("oui", "oui", "oui", "non", "non")
#' )
#' 
#' datf2 <- data.frame(
#'         "id1"=c(1, 2, 3, 7, 9),
#'         "var1"=c("oui2", "oui2", "oui2", "non2", "non2")
#' )
#' 
#' print(left_all(datf1, datf2, datf2, datf2, keep_val=FALSE, id_v="id1"))
#' 
#'   id1 var1.x var1.y var1.x.x var1.y.y
#' 1   1    oui   oui2     oui2     oui2
#' 2   2    oui   oui2     oui2     oui2
#' 3   3    oui   oui2     oui2     oui2
#' 4   4    non   <NA>     <NA>     <NA>
#' 5   5    non   <NA>     <NA>     <NA>#'
#' print(left_all(datf1, datf2, datf2, keep_val=FALSE, id_v="id1"))
#' 
#'   id1 var1.x var1.y var1
#' 1   1    oui   oui2 oui2
#' 2   2    oui   oui2 oui2
#' 3   3    oui   oui2 oui2
#' 4   4    non   <NA> <NA>
#' 5   5    non   <NA> <NA>
#' 
#' @export

left_all <- function(..., keep_val=FALSE, id_v){
        cur_lst <- list(...)
        rtn_dt <- as.data.frame(cur_lst[1])
        if (length(list(...)) > 1){
                for (el in 2:length(cur_lst)){
                  rtn_dt <- left_join(
                                x = rtn_dt,
                                y = as.data.frame(cur_lst[el]),
                                by = id_v,
                                keep = keep_val
                  )
                }
        }
  return(rtn_dt)
}

#' inner_all
#'
#' Allow to apply inner join on n dataframes, datatables, tibble
#'
#' @param ... are all the dataframes etc
#' @param keep_val is if you want to keep the id column
#' @param id_v is the common id of all the dataframes etc
#' @examples
#'
#' datf1 <- data.frame(
#'         "id1"=c(1:5),
#'         "var1"=c("oui", "oui", "oui", "non", "non")
#' )
#' 
#' datf2 <- data.frame(
#'         "id1"=c(1, 2, 3, 7, 9),
#'         "var1"=c("oui2", "oui2", "oui2", "non2", "non2")
#' )
#' 
#' print(inner_all(datf1, datf2, keep_val=FALSE, id_v="id1"))
#' 
#' id1 var1.x var1.y
#' 1   1    oui   oui2
#' 2   2    oui   oui2
#' 3   3    oui   oui2
#'
#' @export

inner_all <- function(..., keep_val=FALSE, id_v){
        cur_lst <- list(...)
        rtn_dt <- as.data.frame(cur_lst[1])
        if (length(list(...)) > 1){
                for (el in 2:length(cur_lst)){
                        print(c(id_v[(el-1)], id_v[el]))
                  rtn_dt <- inner_join(
                                x = rtn_dt,
                                y = as.data.frame(cur_lst[el]),
                                by = id_v,
                                keep = keep_val
                  )
                }
        }
  return(rtn_dt)
}

