extreme_rank_length <- function(dt, type = c("two_sided", "one_sided_left", "one_sided_right")){
  type <- match.arg(type)
  dm <- dim(dt)
  n <- dm[1]; p <- dm[2]
  rank_matrix_forward <- apply(dt, 2L, rank) # low values have low ranks

  if(type == "two_sided"){ # both low and high are extreme
    rank_matrix_backward <- n+1-rank_matrix_forward
    rank_ij <- pmin(rank_matrix_forward, rank_matrix_backward)

    # import cpp code to compute depth
  } else if (type == "one_sided_left"){# both

  }

}
