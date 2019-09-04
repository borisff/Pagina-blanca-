#Funcion para calcular el digito verificador de los rut 

dv <- function(rut){
  rut = as.character(rut)
  x = as.numeric(rev(strsplit(rut,NULL)[[1]]))
  Multiplo = rep(2:7,length.out=length(x))
  y = sum(x*Multiplo)
  z = 11 - y + floor(y/11)*11
  key = c(1:11)
  val = c(1:9,"k",0)
  dv = val[match(z, key)]
  return(dv)
}

#funcion para crear rut desde 2.000.000 hasta los 30.000.000

for( i in 2000000:99999999){
  print(paste(i, dv(i)))
}

#intentamos entrar a la pagina

url_nueva <- 'http://www.blancas.uchile.cl/resultado.php'

consultaGetNuevas <- GET(url = url_nueva, encode = "json", add_headers('x-xsrf-token' = XXSRFToken, Cookie = Cookie),encode = "json")
