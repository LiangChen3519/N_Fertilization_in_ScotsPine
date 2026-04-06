using RCall

# reference from 
    #https://github.com/lypluo/Greenness_and_Treering/blob/main/R/Water_availbilty_index/Cal_WAI.R
# We are using macro way in Julia to transform R function for speeding up
SWI(preci, pet) = begin rcopy(R"""
        library(zoo)
        theta = 0.05
        awc = 100
        n = length($preci)
        WAI = rep(NA, n) 
        WAI[1] = awc 
        for(i in 1:n){
            if(is.na($preci[i]) | $preci[i]<0){
              $preci[i]=0
            }
          }
        pet = na.fill($pet,c(NA,'extend',NA))
        ETmodWAI = rep(NA, n) 
        input = rep(NA, n) 
        for (i in 1:(n-1)) { 
            input[i] <- min($preci[i], awc - WAI[i]) 
            ETsupply <- theta * (WAI[i]+input[i]) 
            ETmodWAI[i] <- min(pet[i], ETsupply) 
    WAI[i+1] <- WAI[i] + input[i] - ETmodWAI[i]
  }
  data.frame(WAI=WAI, ETmodWAI=ETmodWAI, input=input)
        """)
end