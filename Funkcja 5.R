kalkulator=function(a, b, znak){
  if(znak=="+"){
    return(a+b)
  }
  if(znak=="-"){
    return(a-b)
  }
  if(znak=="*"){
    return(a*b)
  }
  if(znak=="/"){
    if(b==0){
      return("Nie ma dzielenia przez 0")
    }
    return(a/b)
  }
  else{
    return("Nieprawidlowy operator")
  }
}