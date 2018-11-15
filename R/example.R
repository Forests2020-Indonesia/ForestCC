# menghitung volume dengan input diameter dan tinggi
volume <- function(d, h)
{
  l <- 0.25 * pi * d^2
  v <- l * h 
  return(v)
}

# jangan lupa mengeksekusi fungsi sebelum digunakan
# (supaya tersedia di lingkungan R)
volume(10, 1)

# perintah dengan hasil yang sama:
volume(d=10, h=1)


# perulangan
X <- c(1,3,5,7)

for(x in X) {
  print(x * 2)
}

# same result
for(i in 1:length(X)) {
  print(X[i] * 2)
}

# if else
bil.genap <- function(x)
{
  if(x %% 2 == 0) 
    print("BENAR")
  else
    print("SALAH")
}

# tambahan