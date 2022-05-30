#Hellwig-Total
#@Autor, napisa?, opisa?, przetestowa?: Napora Jaros?aw

#dane najpierw x pozniej y

#----- Hellwig z danych ----
library(readxl)
Dane=read_excel("Excel_obliczenia.xlsx", sheet=2, range="A1:I18") #zmie? zakresy

N=length(Dane)     #oblicza ilo?? zmiennych obja?niaj?cych
M=2^N-1              #oblicza ilo?? kombinacji 0-1


zm_obj=Dane[,1:N]    #tworzy macierz zmiennych obja?nij?cych
r=cor(zm_obj)               #tworzy macierz korelacji mi?dzy zmiennymi
r=as.matrix(abs(r))         #tworzy warto?ci bezwzgledne i macierz 
R=cor(zm_obj,dane_y)    #tworzy wektor korelacji Y z kazd? ze zmiennych
R=as.vector(R)              #zapisuje wektor jako wektor

tab=as.matrix(expand.grid(rep(list(0:1), N)))[-1,]     #tworzy macierz 0-1 kombinacji
colnames(tab)=colnames(Dane)[1:N]
wyniki=matrix(0,M,N)        #tworzy macierz 0 na wyniki cz?stkowe pojemnosci
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

#Podgl?d kilku najlepszych wynik?w
wynikiS=cbind(wyniki,0)
wynikiS[,(N+1)]=rowSums(wyniki)
nazwy=colnames(as.data.frame(wyniki))
colnames(wynikiS)=c(nazwy,"hellwigP")

ind=order(wynikiS[,(N+1)],decreasing = TRUE)[1:15] #zwr?ci 15 najleprzych
najlepsze15=wynikiS[ind,]

