---
categories:
- ""
- ""
date: "2017-10-31T22:42:51-05:00"
description: R_studio.
draft: false
image: R_studio.jpg
keywords: ""
slug: R_studio
title: Something I learnt in this class
---

In this class, I learnt a lot both in class and through group assignments and projects.
Here is one of the problem that I solved: a ggplot that looks very nice:)

```{r}
approval_unique<-approval_polllist %>%  
  #generate new column shows week of enddate and approval rate
  mutate(week_of_year=week(mdy(enddate)),
         Approval_Rate=approve-disapprove) 

approval_sort<-approval_unique %>% #calculate the data needed for mean and CI
  group_by(week_of_year) %>% 
  summarise(mean_rate = mean(Approval_Rate, na.rm=TRUE),
            sd_rate = sd(Approval_Rate, na.rm=TRUE),
            count = n(),
            se_rate = sd_rate/sqrt(count),
            lower_cl = mean_rate - qnorm(0.975) * se_rate,
            upper_cl = mean_rate + qnorm(0.975) * se_rate)

ggplot(approval_sort,aes(x=week_of_year,y=mean_rate))+
  geom_point(color="red",size=2)+
  theme_bw()+ #change the background
  
  geom_hline(aes(yintercept=0),
             linetype = "solid",color="orange",size=1.5)+  #create the orange line 
  geom_line(color="red")+ #connect the points
  
  scale_x_continuous(limits=c(8,40))+ 
  scale_y_continuous(limits=c(-10,20),
                     breaks=seq(-10,15,by=2.5))+
  
  theme(axis.ticks = element_blank(), #remove ticks and border
        panel.border = element_blank(),)+
  
  labs(title="Estimating Approval Margin(approve-disapprove) for Joe Bidden",
       subtitle="Weekly average of all polls",
       x="Week of the year",
       y="Average Approval Margin(Approve-Disapprove")+
  
  geom_ribbon(aes(ymin=lower_cl,ymax=upper_cl), #draw the CI and fill it
              color="red",
              fill="grey",alpha=0.3)+
  
  geom_smooth(color="blue",se=FALSE)+ #draw the smooth line
  annotate("text",x=22,y=20,label="2021")

```

![Biden Task](https://raw.githubusercontent.com/Siqiong-zhou/my_website/main/themes/forty/static/img/r_graph.png)