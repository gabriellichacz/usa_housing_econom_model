#Hellwig-Total
#@Autor, napisa³, opisa³, przetestowa³: Napora Jaros³aw

#dane najpierw x pozniej y

#----- Hellwig z danych ----
library(readxl)
Dane=read_excel("Obliczenia.xlsx", sheet=3,range="A1:E14") #zmieñ zakresy

N=length(Dane)-1     #oblicza iloœæ zmiennych objaœniaj¹cych
M=2^N-1              #oblicza iloœæ kombinacji 0-1


zm_obj=Dane[,1:N]    #tworzy macierz zmiennych objaœnij¹cych
r=cor(zm_obj)               #tworzy macierz korelacji miêdzy zmiennymi
r=as.matrix(abs(r))         #tworzy wartoœci bezwzgledne i macierz 
R=cor(zm_obj,Dane[,N+1])    #tworzy wektor korelacji Y z kazd¹ ze zmiennych
R=as.vector(R)              #zapisuje wektor jako wektor

tab=as.matrix(expand.grid(rep(list(0:1), N)))[-1,]     #tworzy macierz 0-1 kombinacji
colnames(tab)=colnames(Dane)[1:N]
wyniki=matrix(0,M,N)        #tworzy macierz 0 na wyniki cz¹stkowe pojemnosci
colnames(wyniki)=colnames(Dane)[1:N]
for(i in 1:M)
{
  for(j in 1:N)
  {
    if(tab[i,j]!=0){wyniki[i,j]=(R[j]^2)/(tab[i,]%*%(as.vector(r[,j])))}
  }
} 

maks=which.max(rowSums(wyniki))
tab[maks,]

#Podgl¹d kilku najlepszych wyników
wynikiS=cbind(wyniki,0)
wynikiS[,(N+1)]=rowSums(wyniki)
nazwy=colnames(as.data.frame(wyniki))
colnames(wynikiS)=c(nazwy,"hellwigP")

ind=order(wynikiS[,(N+1)],decreasing = TRUE)[1:15] #zwróci 15 najleprzych
najlepsze15=wynikiS[ind,]

