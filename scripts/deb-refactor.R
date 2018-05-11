
deb_decimal_check <- function(l, s, d) {
  # Check if the value is positive
  # Return positive values so only need to use floor
  if (l + s/20 + d/240 < 0) {
    l <- -l
    s <- -s
    d <- -d
  }
  # Check for decimals in l
  if (l != round(l)) {
    temp_s <- s + (l - floor(l)) * 20
    l <- floor(l)
    if (temp_s != round(temp_s)) {
      s <- floor(temp_s)
      d <- d + (temp_s - floor(temp_s)) * 12
    } else {
      s <- temp_s
    }
  }
  # Check for decimals in s
  if (s != round(s)) {
    d <- d + (s - floor(s)) * 12
    s <- floor(s)
  }
  c(l, s, d)
}


deb_librae <- function(l, s, d) {
  lsd <- deb_decimal_check(l, s, d)
  librae <- lsd[1]
  solidi <- lsd[2]
  denarii <- lsd[3]
  librae <- librae + ((solidi + denarii %/% 12) %/% 20)
  if (l + s/20 + d/240 > 0) {
    librae
  } else {
    -librae
  }
}

deb_solidi <- function(l, s, d) {
  lsd <- deb_decimal_check(l, s, d)
  solidi <- lsd[2]
  denarii <- lsd[3]
  solidi <- (solidi + denarii %/% 12) %% 20
  if (l + s/20 + d/240 > 0) {
    solidi
  } else {
    -solidi
  }
}

deb_denarii <- function(l, s, d, x) {
  lsd <- deb_decimal_check(l, s, d)
  denarii <- lsd[3]
  denarii <- round(denarii %% 12, x)
  if (l + s/20 + d/240 > 0) {
    denarii
  } else {
    -denarii
  }
}

deb_refactor <- function(l, s, d, x = 3, vector = FALSE) {
  librae <- deb_librae(l, s, d)
  solidi <- deb_solidi(l, s, d)
  denarii <- deb_denarii(l, s, d, x)
  if (vector == FALSE) {
    tibble::tibble(
      l = librae,
      s = solidi,
      d = denarii)
  } else {
    c(
      l = librae,
      s = solidi,
      d = denarii)
  }
}
