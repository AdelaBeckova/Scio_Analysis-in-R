library(dplyr)
library(tidyverse)
library(magrittr)
library(ggplot2)


#počet zaměstnanců firmy

only_hard$`@pocet_zamestnancu` <- factor(only_hard$`@pocet_zamestnancu`, levels = c("do 20","20-50","50-250","250-1000","více než 1000"))

customers_test_only$`@pocet_zamestnancu` <- factor(customers_test_only$`@pocet_zamestnancu`, levels = c("do 20","20-50","50-250","250-1000","více než 1000"))

neoveruji<-customers%>%filter(`@overuji_znalosti_ve_vr`=="ne")

only_hard%>%drop_na(`@pocet_zamestnancu`)%>%
ggplot(aes(x=`@pocet_zamestnancu`,fill=`@pocet_zamestnancu`)) + 
  geom_bar(stat="count")+
  theme_bw()+
  guides(fill=guide_legend(title="Počet zaměstnanců"))+
  labs(x="N=86",y="")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.3)+
  scale_fill_manual(breaks=c("do 20","20-50","50-250","250-1000","více než 1000"),
                    values=c("#ACCDEB",
                             "#1977BD",
                             "#E0E1E3",
                             "#6C6D70",
                             "#231F20"))+
  ggtitle("Počet zaměstnanců těch, co ověřují hard skills")+
  theme(legend.position = "none")

#počet vř ve firmě

only_hard$`@kolik_vr` <- factor(only_hard$`@kolik_vr`, levels = c("0-10","10-50","50-100","100 a více","nevím"))

only_hard%>%drop_na(`@kolik_vr`)%>%
ggplot(aes(x=`@kolik_vr`,fill=`@kolik_vr`)) +
  geom_bar(stat="count")+
  theme_bw()+
  guides(fill=guide_legend(title="Počet VŘ"))+
  labs(x="N=86",y="")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.3)+
  scale_fill_manual(breaks=c("0-10","10-50","50-100","100 a více","nevím"),
                    values=c("#1977BD",
                             "#ACCDEB",
                             "#E0E1E3",
                             "#6C6D70",
                             "#231F20"))+
  ggtitle("Počet VŘ těch, co ověřují hard skills")+
  theme(legend.position = "none")

#řešíte podvádění?
only_hard%>%drop_na(`@resite_podvadeni`)%>%
  ggplot(aes(x=`@resite_podvadeni`,fill=`@resite_podvadeni`)) + 
  geom_bar(stat="count")+
  theme_bw()+
  guides(fill=guide_legend(title="Řešíte podvádění?"))+
  labs(x="N=89",y="")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.3)+
  scale_x_discrete(labels = NULL, breaks = NULL)+
  scale_fill_manual(values=c("#1977BD",
                             "#ACCDEB",
                             "#E0E1E3",
                             "#6C6D70"))+
  ggtitle("Řeší ti, co ověřují hard skills, podvádění?")

zab_podv<-customers%>%filter(`@resite_podvadeni`=="Ano, zabýváme se tím.")
  
#ověřujete znalosti ve vř?
customers%>%drop_na(`@overuji_znalosti_ve_vr`)%>%
  ggplot(aes(x=`@overuji_znalosti_ve_vr`,fill=`@overuji_znalosti_ve_vr`)) + 
  geom_bar(stat="count")+
  theme_bw()+
  guides(fill=guide_legend(title="Ověřujete znalosti ve VŘ?"))+
  labs(x="N=174",y="")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.3)+
  scale_fill_manual(values=c("#1977BD",
                             "#ACCDEB",
                             "#E0E1E3"))+
  theme(legend.position = "none")+
  ggtitle("Ověřují znalosti ve VŘ?")

#ověřují...zkoušeli testy?
only_hard%>%drop_na(`@overuji_zkouseli_testy`)%>%
ggplot(aes(x=`@overuji_zkouseli_testy`,fill=`@overuji_zkouseli_testy`)) + 
  geom_bar(stat="count")+
  theme_bw()+
  guides(fill=guide_legend(title="Ověřujete-li znalosti ve VŘ...zkoušeli jste testy?"))+
  labs(x="N=67",y="")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.3)+
  scale_x_discrete(labels = NULL, breaks = NULL)+
  scale_fill_manual(values=c("#1977BD",
                             "#ACCDEB",
                             "#E0E1E3",
                             "#6C6D70",
                             "#231F20"))+
  ggtitle("Ti, co ověřují hard skills, zkoušeli testy?")
  

#neověřují...zkoušeli testy?
customers%>%drop_na(`@neoveruji_zkouseli_testy`)%>%
ggplot(aes(x=`@neoveruji_zkouseli_testy`,fill=`@neoveruji_zkouseli_testy`))+
  geom_bar(stat="count")+
  theme_bw()+
  guides(fill=guide_legend(title="Sice ve VŘ nyní neověřujete znalosti, ale... zkoušeli jste testy?"))+
  labs(x="N=21",y="")+geom_text(stat='count', aes(label=..count..), vjust=-0.3)+
  scale_x_discrete(labels = NULL, breaks = NULL)+
  scale_fill_manual(values=c("#1977BD",
                             "#ACCDEB",
                             "#E0E1E3",
                             "#6C6D70"))

#dělá vř?
customers%>%drop_na(`@options_delavr`)%>%
  ggplot(aes(x=`@options_delavr`,fill=`@options_delavr`)) + 
  geom_bar(stat="count")+
  theme_bw()+
  guides(fill=guide_legend(title="Dělá VŘ"))+
  labs(x="N=182",y="")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.3)+
  scale_fill_manual(values=c("#1977BD",
                             "#ACCDEB"))+
  theme(legend.position = "none")+
  ggtitle("Dělá VŘ?")

#data-skills

ggplot(data=moje_data, aes(x=reorder(skills, -numbers),y=numbers,fill=skills, label=numbers)) +
  geom_col()+
  labs(y="",x="N=135")+
  theme_bw()+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_manual(breaks=c("softskills","hardskills","psycho"),values=c("#1977BD","#ACCDEB","#E0E1E3"))+
  theme(legend.position = "none")

#data-convertcsv - neověřují, proč ne testy
ggplot(data=convertcsv, aes(x=fct_reorder(value,-number),y=number,label=number)) +
  geom_col(fill = "#ACCDEB")+
  theme_bw()+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  coord_flip()+
  guides(fill = FALSE)+
  labs(x="",y="N=25")+
  ggtitle("Neověřují, proč nepoužívají testy")
  

#kdo vytváří zadání
only_hard%>%
  drop_na(`@zadani_testu_kdo`)%>%
  ggplot(aes(x=`@zadani_testu_kdo`,fill=`@zadani_testu_kdo`)) + 
  geom_bar(stat="count")+theme_bw()+
  guides(fill=guide_legend(title="Kdo vám vytváří zadání?"))+
  labs(x="N=23",y="")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.3)+
  scale_x_discrete(labels = NULL, breaks = NULL)+
  scale_fill_manual(values=c("#1977BD",
                           "#ACCDEB",
                           "#E0E1E3",
                           "#6C6D70",
                           "#231F20"))

#odvětví
only_hard%>%
  drop_na(`@odvetvi`)%>%
  ggplot(aes(x=reorder(`@odvetvi`, `@odvetvi`, function(x)-length(x)))) +
  geom_bar(fill = "#ACCDEB") +
  labs(y="N=86",x="")+
  geom_text(stat='count', aes(label=..count..), hjust=-0.2)+
  theme_bw()+
  coord_flip()+
  ggtitle("Odvětví těch, co ověřují hard skills")



#neověřují-li - odvětví
neoveruji<-customers%>%filter(`@overuji_znalosti_ve_vr`=="ne")
customers_test_only%>%
  drop_na(`@odvetvi`)%>%
  ggplot(aes(x=reorder(`@odvetvi`, `@odvetvi`, function(x)-length(x)))) +
  geom_bar(fill = "#ACCDEB") +
  labs(y="N=21",x="")+
  geom_text(stat='count', aes(label=..count..), hjust=-0.2)+
  theme_bw()+
  coord_flip()+
  ggtitle("Odvětví těch, kteří ve VŘ využívají formu testů")

#dataset:dataset - Jakým způsobem ověřují

ggplot(data=only_hard_dataset, aes(x=fct_reorder(value,-number),y=number,label=number)) +
  geom_col(fill = "#ACCDEB")+
  theme_bw()+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  coord_flip()+
  guides(fill = FALSE)+
  labs(x="",y="N=86")+
  ggtitle("Jakou formou ověřují ti, co ověřují hard skills")

#dataset:dataset2 - Proč nepoužívají testy
ggplot(data=dataset3, aes(x=fct_reorder(value,-number),y=number,label=number)) +
  geom_col(fill = "#ACCDEB")+
  theme_bw()+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  coord_flip()+
  guides(fill = FALSE)+
  labs(x="",y="N=88")+
  ggtitle("Proč nepoužíváte testy")

#dataset-skills4
skills5<-skills5%>%
  mutate(status = case_when((endsWith(A, "d") & endsWith(B,"t") & endsWith(C,"o"))  ~ "hard+soft+psycho",
                            (endsWith(B,"t") & endsWith(C,"o"))~"soft+psycho",
                            (endsWith(A, "d") & endsWith(C,"o"))~"hard+psycho",
                            (endsWith(A, "d") & endsWith(B,"t"))~"hard+soft",
                            (endsWith(A, "d"))~"hard",
                            (endsWith(B,"t"))~"soft",
                            (endsWith(C,"o"))~"psycho"))

skills5$status <- factor(skills5$status, levels = c("hard+psycho", "psycho","soft+psycho","soft","hard+soft+psycho","hard","hard+soft"))

skills5%>%
  drop_na(status)%>%
  ggplot(aes(x=reorder(status,status, function(x)-length(x)),fill=status)) +
  geom_bar(stat="count")+theme_bw()+
  guides(fill=guide_legend(title="Co ověřujete?"))+
  labs(x="",y="N=135")+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)+
  scale_x_discrete(labels = NULL, breaks = NULL)+
  scale_fill_manual(breaks=c("soft+psycho","psycho","hard+psycho","soft","hard+soft+psycho","hard","hard+soft"),values=c("#ACCDEB","#AAB8BF","#1977BD","#6C6D70","#231F20","#E1E5FA","#A2A2F2"))

#piechart
slices<-c(15,3,3,1,1)
labels<-c("15","3","3","1","1")
colors=c("#1977BD","#ACCDEB","#E0E1E3","#6C6D70","#231F20")
pie(slices, labels = labels, main="Pie Chart",col=colors)
legend(.9, .1, c("Vytváříme si vlastní, ale zvažujeme využití externích","Máme připravené testy od externího dodavatele","Vytváříme si vlastní, ale zvažujeme využití externích","Máme kombinaci vlastních a externích zadání","Nevím"), cex = 0.7, fill = colors)
