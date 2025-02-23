podatek=function(kwota, rozliczenie){
  if(rozliczenie=="liniowe"){
    return(0.19*kwota)
  }
  if(rozliczenie=="ogolne"){
    if(kwota<=85528){
      return(0.18*kwota - 556)
    }
    else{
      return(14839 + 0.32*(kwota-85528))
    }
  }
}