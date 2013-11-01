###############################
####HEALTH#####################
###############################
h <- readRDS("~/Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/999cleaned/Health_661_999Cleaned.rds")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
library("ggplot2")

"cells<-" = function(df,cols,rows,value) {
  for (r in rows) { for (c in cols) { df[[r, c]] <- value } }
  df
}

################################
####old scripts####   
################################

#num_doctors_active is greater than 12 for all facilities other than teachinghospital and federalmedical center
h_medical_staff_active.num_doctors_active <- subset(h, 
                                                    select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.num_doctors_active),
                                                    subset=((h$facility_type != "teachinghospital" & 
                                                      h$facility_type != "federalmedicalcentre") & 
                                                      medical_staff_active.num_doctors_active > 12))
  #cleaning
    cells(h, 'medical_staff_active.num_doctors_active',
          which(h$medical_staff_active.num_doctors_active > 12 & 
            (h$facility_type != "teachinghospital" & 
            h$facility_type != "federalmedicalcentre"))) <- NA
 
#num_doctors_active is greater than 20 for teachinghospital and federalmedicalcenter
h_medical_staff_active.num_doctors_active_2 <- subset(h, 
                                                    select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.num_doctors_active),
                                                    subset=((h$facility_type == "teachinghospital" | 
                                                      h$facility_type == "federalmedicalcentre") & 
                                                      medical_staff_active.num_doctors_active > 20))

    #cleaning
    cells(h, 'medical_staff_active.num_doctors_active',
          which(h$medical_staff_active.num_doctors_active > 20 & 
            (h$facility_type == "teachinghospital" | 
            h$facility_type == "federalmedicalcentre"))) <- NA

#num_nurses_active is greater than 16 for all facilities except teaching & federal
h_medical_staff_active.num_nurses_active <- subset(h, 
                                                    select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.num_nurses_active),
                                                    subset=((h$facility_type != "teachinghospital" & 
                                                      h$facility_type != "federalmedicalcentre") & 
                                                      medical_staff_active.num_nurses_active > 16))

    #cleaning
    cells(h, 'medical_staff_active.num_nurses_active',
          which(h$medical_staff_active.num_nurses_active > 16 & 
            (h$facility_type != "teachinghospital" & 
            h$facility_type != "federalmedicalcentre"))) <- NA

#num_nurses_active is greater than 24 for teaching & federal
h_medical_staff_active.num_nurses_active_2 <- subset(h, 
                                                      select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.num_nurses_active),
                                                      subset=((h$facility_type == "teachinghospital" | 
                                                        h$facility_type == "federalmedicalcentre") & 
                                                        medical_staff_active.num_nurses_active > 24))

    #cleaning
    cells(h, 'medical_staff_active.num_nurses_active',
          which(h$medical_staff_active.num_nurses_active > 24 & 
            (h$facility_type == "teachinghospital" | 
            h$facility_type == "federalmedicalcentre"))) <- NA

#num_midwives_active is greater than 16 for all facilities except teaching & federal
h_medical_staff_active.num_midwives_active <- subset(h, 
                                                   select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.num_midwives_active),
                                                   subset=((h$facility_type != "teachinghospital" & 
                                                     h$facility_type != "federalmedicalcentre") & 
                                                     medical_staff_active.num_midwives_active > 16))

    #cleaning
    cells(h, 'medical_staff_active.num_midwives_active',
          which(h$medical_staff_active.num_midwives_active > 16 & 
            (h$facility_type != "teachinghospital" & 
            h$facility_type != "federalmedicalcentre"))) <- NA

#num_midwives_active is greater than 24 for teaching & federal
h_medical_staff_active.num_midwives_active_2 <- subset(h, 
                                                     select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.num_midwives_active),
                                                     subset=((h$facility_type == "teachinghospital" | 
                                                       h$facility_type == "federalmedicalcentre") & 
                                                       medical_staff_active.num_midwives_active > 24))

    #cleaning
    cells(h, 'medical_staff_active.num_midwives_active',
          which(h$medical_staff_active.num_midwives_active > 24 & 
            (h$facility_type == "teachinghospital" | 
            h$facility_type == "federalmedicalcentre"))) <- NA

#num_nursemidwives_active is greater than 16 for all facilities except teaching & federal
h_medical_staff_active.num_nursemidwives_active_active <- subset(h, 
                                                     select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.num_nursemidwives_active),
                                                     subset=((h$facility_type != "teachinghospital" & 
                                                       h$facility_type != "federalmedicalcentre") & 
                                                       medical_staff_active.num_nursemidwives_active > 16))

    #cleaning
    cells(h, 'medical_staff_active.num_nursemidwives_active',
          which(h$medical_staff_active.num_nursemidwives_active > 16 & 
            (h$facility_type != "teachinghospital" & 
            h$facility_type != "federalmedicalcentre"))) <- NA

#num_nursemidwives_active is greater than 24 for teaching & federal
h_medical_staff_active.num_nursemidwives_active_2 <- subset(h, 
                                                       select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.num_nursemidwives_active),
                                                       subset=((h$facility_type == "teachinghospital" | 
                                                         h$facility_type == "federalmedicalcentre") & 
                                                         medical_staff_active.num_nursemidwives_active > 24))
    
    #cleaning
    cells(h, 'medical_staff_active.num_nursemidwives_active',
          which(h$medical_staff_active.num_nursemidwives_active > 24 & 
            (h$facility_type == "teachinghospital" | 
            h$facility_type == "federalmedicalcentre"))) <- NA

#num_chews_active is greater than 50
h_medical_staff_active.num_chews_active <- subset(h, 
                                                                 select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.num_chews_active),
                                                                 subset=(medical_staff_active.num_chews_active > 50))
    #cleaning
    cells(h, 'medical_staff_active.num_nursemidwives_active',
          which(h$medical_staff_active.num_nursemidwives_active > 50)) <- NA

#num_jrchews_active is greater than 50
h_medical_staff_active.num_junior_chews_active <- subset(h, 
                                                  select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.num_junior_chews_active),
                                                  subset=(medical_staff_active.num_junior_chews_active > 50))
#cleaning
cells(h, 'medical_staff_active.num_junior_chews_active',
      which(h$medical_staff_active.num_junior_chews_active > 50)) <- NA



###############################
########graphs/tables##########
###############################

##not_for_private_1.days_no_electricity                                      
    #by zone
    ggplot(h, aes(x=mylga_zone, y=not_for_private_1.days_no_electricity, fill=mylga_zone)) + coord_flip() + 
      geom_boxplot() + ylab('Days No electricity') + xlab('Zone') + scale_fill_manual(values=cbPalette)
    #cleaning
    #h_not_for_private_1.days_no_electricity <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, not_for_private_1.days_no_electricity), not_for_private_1.days_no_electricity < )

##not_for_private_1.days_no_potable_water_pastmth                            
    #by zone
    ggplot(h, aes(x=mylga_zone, y=not_for_private_1.days_no_potable_water_pastmth, fill=mylga_zone)) + coord_flip() + 
      geom_boxplot() + ylab('Days No Potable Water Past Month') + xlab('Zone') + scale_fill_manual(values=cbPalette)
    #cleaning
    #h_not_for_private_1.days_no_potable_water_pastmth <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, not_for_private_1.days_no_electricity, not_for_private_1.days_no_potable_water_pastmth), not_for_private_1.days_no_potable_water_pastmth < )

##medical_staff_posted.num_doctors_posted    => 12    / 20                             
    #by zone
    ggplot(h, aes(x=mylga_zone, y=medical_staff_posted.num_doctors_posted, fill=mylga_zone)) + coord_flip() + 
      geom_boxplot() + ylab('Number of Doctors Posted') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) + scale_y_continuous(limits=c(0,200))
    #by facility type and zone
    ggplot(h, aes(x=facility_type, y=medical_staff_posted.num_doctors_posted, fill=mylga_zone)) + coord_flip() + 
    geom_boxplot() + ylab('Number of Doctors Posted') + xlab('Facility Type') + 
    scale_fill_manual(values=cbPalette) + scale_y_continuous(limits=c(0,200))  
    #log 10 
    ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_posted.num_doctors_posted), fill=mylga_zone)) + coord_flip() + 
      geom_boxplot() + ylab('Number of Doctors Posted') + xlab('Zone') + scale_fill_manual(values=cbPalette)
    #cleaning
    #h_medical_staff_posted.num_doctors_posted <- subset(h, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_posted.num_doctors_posted), medical_staff_posted.num_doctors_posted > 12 )
h_medical_staff_posted.num_doctors_posted <- subset(h, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_posted.num_doctors_posted), 
                                                    subset=((h$facility_type != "teachinghospital" & 
                                                      h$facility_type != "federalmedicalcentre") & medical_staff_posted.num_doctors_posted > 12 ))


##medical_staff_posted.num_midwives_posted     => 16   / 24                           
    #by zone
    ggplot(h, aes(x=mylga_zone, y=medical_staff_posted.num_midwives_posted, fill=mylga_zone)) + coord_flip() + 
      geom_boxplot() + ylab('Number of Midwives Posted') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
    #log 10 
      ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_posted.num_midwives_posted), fill=mylga_zone)) + coord_flip() + 
      geom_boxplot() + ylab('Number of Midwives Posted') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
    #cleaning
    #h_medical_staff_posted.num_midwives_posted <- subset(h, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_posted.num_midwives_posted), medical_staff_posted.num_midwives_posted > 16 )

##medical_staff_posted.num_nurses_posted > 16/ 24                                    
    #by zone
    ggplot(h, aes(x=mylga_zone, y=medical_staff_posted.num_nurses_posted, fill=mylga_zone)) + coord_flip() + 
      geom_boxplot() + ylab('Number of Nurses Posted') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
    #log 10 
    ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_posted.num_nurses_posted), fill=mylga_zone)) + coord_flip() + 
      geom_boxplot() + ylab('Number of Nurses Posted') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
    #cleaning
    h_medical_staff_posted.num_nurses_posted <- subset(h, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_posted.num_nurses_posted), 
      subset=((h$facility_type != "teachinghospital" & 
        h$facility_type != "federalmedicalcentre") & medical_staff_posted.num_nurses_posted > 16 ))
  h_medical_staff_posted.num_nurses_posted <- subset(h, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_posted.num_nurses_posted), 
                                                   subset=((h$facility_type == "teachinghospital" | 
                                                     h$facility_type == "federalmedicalcentre") & medical_staff_posted.num_nurses_posted > 0 ))

##medical_staff_posted.num_nursemidwives_posted                              
  #by zone
  ggplot(h, aes(x=mylga_zone, y=medical_staff_posted.num_nursemidwives_posted, fill=mylga_zone)) + coord_flip() + 
    geom_boxplot() + ylab('Number of Nurse-Midwives Posted') + xlab('Zone') + 
    scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
  #log 10 
  ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_posted.num_nursemidwives_posted), fill=mylga_zone)) + coord_flip() + 
    geom_boxplot() + ylab('Number of Nurse-Midwives Posted') + xlab('Zone') + 
    scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
  #cleaning
  #h_medical_staff_posted.num_nursemidwives_posted <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_posted.num_nursemidwives_posted), medical_staff_posted.num_nursemidwives_posted < )

##medical_staff_posted.num_cho_posted                                        
    #by zone
    ggplot(h, aes(x=mylga_zone, y=medical_staff_posted.num_cho_posted, fill=mylga_zone) + coord_flip() + 
      geom_boxplot() + ylab('Number of CHOs Posted') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
    #log 10 
    ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_posted.num_cho_posted), fill=mylga_zone)) + coord_flip() + 
      geom_boxplot() + ylab('Number of CHOs Posted') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
    #cleaning
    #h_medical_staff_posted.num_cho_posted <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_posted.num_cho_posted), medical_staff_posted.num_cho_posted < )

##medical_staff_posted.num_chews_posted         
    #by zone
    ggplot(h, aes(x=mylga_zone, y=medical_staff_posted.num_chews_posted, fill=mylga_zone)) + coord_flip() + 
      geom_boxplot() + ylab('Number of CHEWs Posted') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
    #log 10 
    ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_posted.num_chews_posted), fill=mylga_zone)) + coord_flip() + 
      geom_boxplot() + ylab('Number of CHEWs Posted') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
    #cleaning
    #h_medical_staff_posted.num_chews_posted <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_posted.num_chews_posted), medical_staff_posted.num_chews_posted < )

##medical_staff_posted.num_junior_chews_posted                               
    #by zone
    ggplot(h, aes(x=mylga_zone, y=medical_staff_posted.num_junior_chews_posted, fill=mylga_zone)) + coord_flip() + 
      geom_boxplot() + ylab('Number of Junior CHEWs Posted') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
    #log 10 
    ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_posted.num_junior_chews_posted), fill=mylga_zone)) + coord_flip() + 
      geom_boxplot() + ylab('Number of Junior CHEWs Posted') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
    #cleaning
    #h_medical_staff_posted.num_junior_chews_posted <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_posted.num_junior_chews_posted), medical_staff_posted.num_junior_chews_posted < )

##medical_staff_posted.pharmacists_posted   
    #by zone
    ggplot(h, aes(x=mylga_zone, y=medical_staff_posted.pharmacists_posted, fill=mylga_zone)) + coord_flip() + 
      geom_boxplot() + ylab('Number of Pharmacists Posted') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #by zone and facility
           ggplot(h, aes(x=facility_type, y=medical_staff_posted.pharmacists_posted, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Pharmacists Posted') + xlab('Facility') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
    #log 10 
    ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_posted.pharmacists_posted), fill=mylga_zone)) + coord_flip() + 
      geom_boxplot() + ylab('Number of Pharmacists Posted') + xlab('Zone') + 
      scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
    #cleaning
    #h_medical_staff_posted.pharmacists_posted <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_posted.pharmacists_posted), medical_staff_posted.pharmacists_posted < )

##medical_staff_posted.environmental_health_officers_posted
           #by zone
           ggplot(h, aes(x=mylga_zone, y=medical_staff_posted.environmental_health_officers_posted, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Environmental Health Officers Posted') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #log 10 
           ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_posted.environmental_health_officers_posted), fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Environmental Health Officers Posted') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_medical_staff_posted.environmental_health_officers_posted <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_posted.environmental_health_officers_posted), medical_staff_posted.environmental_health_officers_posted < )
           
##medical_staff_posted.lab_technicians_posted 
           #by zone
           ggplot(h, aes(x=mylga_zone, y=medical_staff_posted.lab_technicians_posted, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Lab Technicians Posted') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #log 10 
           ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_posted.lab_technicians_posted), fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Lab Technicians Officers Posted') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_medical_staff_posted.lab_technicians_posted <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_posted.lab_technicians_posted), medical_staff_posted.lab_technicians_posted < )
           
##medical_staff_posted.pharma_technicians_posted  
           #by zone
           ggplot(h, aes(x=mylga_zone, y=medical_staff_posted.pharma_technicians_posted, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Pharma-Technicians Posted') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #log 10 
           ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_posted.pharma_technicians_posted), fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Pharma-Technicians Officers Posted') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_medical_staff_posted.pharma_technicians_posted <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_posted.pharma_technicians_posted), medical_staff_posted.pharma_technicians_posted < )
            
##medical_staff_posted.medical_records_officers_posted                       
           #by zone
           ggplot(h, aes(x=mylga_zone, y=medical_staff_posted.pharma_technicians_posted, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Pharma-Technicians Posted') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #log 10 
           ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_posted.pharma_technicians_posted), fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Pharma-Technicians Officers Posted') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_medical_staff_posted.pharma_technicians_posted <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_posted.pharma_technicians_posted), medical_staff_posted.pharma_technicians_posted < )
           
##medical_staff_active.num_doctors_active  
           #by zone
           ggplot(h, aes(x=mylga_zone, y=medical_staff_active.num_doctors_active, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Doctors Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #log 10 
           ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_active.num_doctors_active), fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Doctors Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_medical_staff_active.num_doctors_active <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.num_doctors_active), medical_staff_active.num_doctors_active < )
                   
##medical_staff_active.num_midwives_active                                   
           #by zone
           ggplot(h, aes(x=mylga_zone, y=medical_staff_active.num_midwives_active, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Midwives Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #log 10 
           ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_active.num_midwives_active), fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Midwives Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_medical_staff_active.num_midwives_active <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.num_midwives_active), medical_staff_active.num_midwives_active < )
                
##medical_staff_active.num_nurses_active                                     
           #by zone
           ggplot(h, aes(x=mylga_zone, y=medical_staff_active.num_nurses_active, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Nurses Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #log 10 
           ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_active.num_nurses_active), fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Nurses Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_medical_staff_active.num_nurses_active <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.num_nurses_active), medical_staff_active.num_nurses_active < )
           
##medical_staff_active.num_nursemidwives_active 
           #by zone
           ggplot(h, aes(x=mylga_zone, y=medical_staff_active.num_nursemidwives_active, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Nurse-Midwives Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #log 10 
           ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_active.num_nursemidwives_active), fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Nurse-Midwives Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_medical_staff_active.num_nursemidwives_active <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.num_nursemidwives_active), medical_staff_active.num_nursemidwives_active < )
           
##medical_staff_active.num_cho_active
           #by zone
           ggplot(h, aes(x=mylga_zone, y=medical_staff_active.num_cho_active, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of CHOs Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #log 10 
           ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_active.num_cho_active), fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of CHOs Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_medical_staff_active.num_cho_active <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.num_cho_active), medical_staff_active.num_cho_active < )
                   
##medical_staff_active.num_chews_active  
           #by zone
           ggplot(h, aes(x=mylga_zone, y=medical_staff_active.num_chews_active, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of CHEWs Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #log 10 
           ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_active.num_chews_active), fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of CHEWs Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_medical_staff_active.num_chews_active <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.num_chews_active), medical_staff_active.num_chews_active < )
                    
##medical_staff_active.num_junior_chews_active
           #by zone
           ggplot(h, aes(x=mylga_zone, y=medical_staff_active.num_junior_chews_active, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Junior CHEWs Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #log 10 
           ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_active.num_junior_chews_active), fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Junior CHEWs Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_medical_staff_active.num_junior_chews_active <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.num_junior_chews_active), medical_staff_active.num_junior_chews_active < )
           
##medical_staff_active.pharmacists_active
           #by zone
           ggplot(h, aes(x=mylga_zone, y=medical_staff_active.pharmacists_active, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Pharmacists Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #log 10 
           ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_active.pharmacists_active), fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Pharmacists Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_medical_staff_active.pharmacists_active <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.pharmacists_active), medical_staff_active.pharmacists_active < )
               
##medical_staff_active.environmental_health_officers_active
           #by zone
           ggplot(h, aes(x=mylga_zone, y=medical_staff_active.environmental_health_officers_active, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Environmental Health Officers Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #log 10 
           ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_active.environmental_health_officers_active), fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Environmental Health Officers Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_medical_staff_active.environmental_health_officers_active <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.environmental_health_officers_active), medical_staff_active.environmental_health_officers_active < )  
           
##medical_staff_active.lab_technicians_active
           #by zone
           ggplot(h, aes(x=mylga_zone, y=medical_staff_active.lab_technicians_active, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Lab Technicians Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #log 10 
           ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_active.lab_technicians_active), fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Lab Technicians Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_medical_staff_active.lab_technicians_active <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.lab_technicians_active), medical_staff_active.lab_technicians_active < )  
           
##medical_staff_active.pharma_technicians_active
           #by zone
           ggplot(h, aes(x=mylga_zone, y=medical_staff_active.pharma_technicians_active, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Pharma-Technicians Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #log 10 
           ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_active.pharma_technicians_active), fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Pharma-Technicians Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_medical_staff_active.pharma_technicians_active <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.pharma_technicians_active), medical_staff_active.pharma_technicians_active < )  
           
##medical_staff_active.medical_records_officers_active
           #by zone
           ggplot(h, aes(x=mylga_zone, y=medical_staff_active.medical_records_officers_active, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Medical Records Officers Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #log 10 
           ggplot(h, aes(x=mylga_zone, y=log10(medical_staff_active.medical_records_officers_active), fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Pharma-Technicians Active') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_medical_staff_active.medical_records_officers_active <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, medical_staff_active.medical_records_officers_active), medical_staff_active.medical_records_officers_active < )  
           
##inpatient_care_num_beds                                                    
           #by zone
           ggplot(h, aes(x=mylga_zone, y=inpatient_care_num_beds, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Beds in Inpatient-Care') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #log 10 
           ggplot(h, aes(x=mylga_zone, y=log10(inpatient_care_num_beds), fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Beds in Inpatient-Care') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_inpatient_care_num_beds <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, inpatient_care_num_beds), inpatient_care_num_beds < )  
           
##not_for_private_1.toilets_available.num_flush_or_pour_flush_piped  
           #by zone
           ggplot(h, aes(x=mylga_zone, y=not_for_private_1.toilets_available.num_flush_or_pour_flush_piped, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Toilets \n(Flush or Pour Flush Piped)') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_not_for_private_1.toilets_available.num_flush_or_pour_flush_piped <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, not_for_private_1.toilets_available.num_flush_or_pour_flush_piped), not_for_private_1.toilets_available.num_flush_or_pour_flush_piped < )  
           
##not_for_private_1.toilets_available.num_flush_other 
           #by zone
           ggplot(h, aes(x=mylga_zone, y=not_for_private_1.toilets_available.num_flush_other, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Toilets \n(Flush Other)') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_not_for_private_1.toilets_available.num_flush_other <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, not_for_private_1.toilets_available.num_flush_other), not_for_private_1.toilets_available.num_flush_other < )  
           
##not_for_private_1.toilets_available.num_vip_latrine 
           #by zone
           ggplot(h, aes(x=mylga_zone, y=not_for_private_1.toilets_available.num_vip_latrine, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Toilets \n(Flush Other)') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_not_for_private_1.toilets_available.num_vip_latrine <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, not_for_private_1.toilets_available.num_vip_latrine), not_for_private_1.toilets_available.num_vip_latrine < )  
           
##not_for_private_1.toilets_available.num_pit_w_slab 
           #by zone
           ggplot(h, aes(x=mylga_zone, y=not_for_private_1.toilets_available.num_pit_w_slab, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Toilets \n(Pit with Slab)') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_not_for_private_1.toilets_available.num_pit_w_slab <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, not_for_private_1.toilets_available.num_pit_w_slab), not_for_private_1.toilets_available.num_pit_w_slab < )  
           
##not_for_private_1.toilets_available.num_open_pit_latrine
           #by zone
           ggplot(h, aes(x=mylga_zone, y=not_for_private_1.toilets_available.num_open_pit_latrine, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Toilets \n(Open Pit Latrine)') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
          #cleaning
           #h_not_for_private_1.toilets_available.num_open_pit_latrine <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, not_for_private_1.toilets_available.num_open_pit_latrine), not_for_private_1.toilets_available.num_open_pit_latrine < )  
           
##not_for_private_1.toilets_available.num_bucket_system
           #by zone
           ggplot(h, aes(x=mylga_zone, y=not_for_private_1.toilets_available.num_bucket_system, fill=mylga_zone)) + coord_flip() + 
             geom_boxplot() + ylab('Number of Toilets \n(Bucket System)') + xlab('Zone') + 
             scale_fill_manual(values=cbPalette) #+ scale_y_continuous(limits=c(0,50))
           #cleaning
           #h_not_for_private_1.toilets_available.num_bucket_system <- subset(e, select=c(uuid, mylga, mylga_state, mylga_zone, facility_type, not_for_private_1.toilets_available.num_bucket_system), not_for_private_1.toilets_available.num_bucket_system < )  
           
################################
#### categorical data ####
################################

##facility_owner_manager.federalgovernment                                   
           ggplot(h, 
                  aes(x=facility_owner_manager.federalgovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)') 
           
##facility_owner_manager.stategovernment      
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
                      
##facility_owner_manager.lga
           ggplot(h, 
                  aes(x=facility_owner_manager.lga, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##facility_owner_manager.private_forprofit                                   
           ggplot(h, 
                  aes(x=facility_owner_manager.private_forprofit, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##facility_owner_manager.charitable_ngo                                      
           ggplot(h, 
                  aes(x=facility_owner_manager.charitable_ngo, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##facility_owner_manager.religious_org                                       
           ggplot(h, 
                  aes(x=facility_owner_manager.religious_org, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##facility_owner_manager.none                                                
           ggplot(h, 
                  aes(x=facility_owner_manager.none, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##private_yn                                                                 
           ggplot(h, 
                  aes(x=private_yn, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##facility_type                                                              
           ggplot(h, 
                  aes(x=facility_type, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + coord_flip()
                        ylab('Percent (%)')
           
##road_yn                                                                    
           ggplot(h, 
                  aes(x=road_yn, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##road_type                                                                  
           ggplot(h, 
                  aes(x=road_type, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + coord_flip()
                        ylab('Percent (%)')
           
##all_weather_road_yn                                                        
           ggplot(h, 
                  aes(x=all_weather_road_yn, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##transport_to_referral                                                      
           ggplot(h, 
                  aes(x=transport_to_referral, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + coord_flip()
                        ylab('Percent (%)')
           
##not_for_private_1.grid_proximity                                           
           ggplot(h, 
                  aes(x=not_for_private_1.grid_proximity, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.local_grid_proximity                                     
           ggplot(h, 
                  aes(x=not_for_private_1.local_grid_proximity, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + coord_flip()
                        ylab('Percent (%)')
           
##not_for_private_1.generator_funct_yn                                       
           ggplot(h, 
                  aes(x=not_for_private_1.generator_funct_yn, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.solar_funct_yn                                           
           ggplot(h, 
                  aes(x=not_for_private_1.solar_funct_yn, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.grid_funct_yn                                            
           ggplot(h, 
                  aes(x=not_for_private_1.grid_funct_yn, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.vaccine_storage_yn                                       
           ggplot(h, 
                  aes(x=not_for_private_1.vaccine_storage_yn, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + coord_flip()
                        ylab('Percent (%)')
           
##not_for_private_1.cold_storage_system_type                                 
           ggplot(h, 
                  aes(x=not_for_private_1.cold_storage_system_type, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + coord_flip()
                        ylab('Percent (%)')
           
##not_for_private_1.days_vaccines_strg_wo_power
           ggplot(h, 
                  aes(x=not_for_private_1.days_vaccines_strg_wo_power, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + coord_flip()
                        ylab('Percent (%)')
           
##not_for_private_1.building_condition                                       
           ggplot(h, 
                  aes(x=not_for_private_1.building_condition, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + coord_flip()
                        ylab('Percent (%)')
           
##not_for_private_1.toilets_yn                                          
           ggplot(h, 
                  aes(x=not_for_private_1.toilets_yn, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.flush_improved_functional_yn                             
           ggplot(h, 
                  aes(x=not_for_private_1.flush_improved_functional_yn, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.flush_unimproved_functional_yn                           
           ggplot(h, 
                  aes(x=not_for_private_1.flush_unimproved_functional_yn, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
        
##not_for_private_1.vip_latrine_functional_yn                                
           ggplot(h, 
                  aes(x=not_for_private_1.vip_latrine_functional_yn, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.slab_pit_latrine_functional_yn                           
           ggplot(h, 
                  aes(x=not_for_private_1.slab_pit_latrine_functional_yn, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.open_pit_latrine_functional_yn                           
           ggplot(h, 
                  aes(x=not_for_private_1.open_pit_latrine_functional_yn, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.bucket_system_functional_yn                              
           ggplot(h, 
                  aes(x=not_for_private_1.bucket_system_functional_yn, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.phone_signal_strength                                    
           ggplot(h, 
                  aes(x=not_for_private_1.phone_signal_strength, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + coord_flip()
                        ylab('Percent (%)')
           
##not_for_private_1.info_tech_functional_landline                            
           ggplot(h, 
                  aes(x=not_for_private_1.info_tech_functional_landline, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + coord_flip()
                        ylab('Percent (%)')
    
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
##not_for_private_1.info_tech_functional_mobile_facility                     
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.info_tech_functional_mobile_staff                        
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.info_tech_functional_computer                            
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.info_tech_functional_internet                            
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.info_tech_functional_printer                             
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##midwivery_service_scheme_yn                                                
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##delivery_services_yn                                                       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##delivery_skilled_birth_247_yn                                              
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##delivery_staff_csection_yn                                                 
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##facility_open_247_yn                                                       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##staff_quarters_yn                                                          
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##staff_paid_1mths_yn                                                        
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##staff_paid_3mths_yn                                                        
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##malaria_treatment_artemisinin                                              
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##tb_treatment_yn                                                            
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##antenatal_care_yn                                                          
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##antenatal_care_services.note_antenatal_care_services                       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##antenatal_care_services.antenatal_care_physical_exam                       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##antenatal_care_services.antenatal_care_tetanus_vac                         
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##antenatal_care_services.antenatal_care_malaria_prlx                        
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##antenatal_care_services.medication_folic_acid                              
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##emergency_obstetrics_yn                                                    
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##emergency_antenatal.note_emergency_antenatal                               
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
           
##emergency_antenatal.emoc_antibiotics_yn                                    
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##emergency_antenatal.emoc_uterotonics_yn                                    
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##emergency_antenatal.emoc_parenteral_anticonvulsant_yn                      
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##emergency_antenatal.emoc_vacuum_extractor_yn                               
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##emergency_antenatal.emoc_sba_placenta_removal_yn                           
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##c_section_yn                                                               
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##cesarean_section.note_cesarean_section                                     
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##cesarean_section.compr_oc_blood_transfusions                               
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##cesarean_section.compr_oc_access_safe_blood                                
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##cesarean_section.compr_oc_anesthesia_machine                               
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##family_planning_yn                                                         
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##family_planning.note_family_planning                                       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##family_planning.family_planning_pill                                       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##family_planning.family_planning_iud                                        
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##family_planning.family_planning_injectables                                
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##family_planning.family_planning_implants                                   
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##family_planning.family_planning_sterilization_m                            
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##family_planning.family_planning_sterilization_f                            
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##inpatient_care_yn                                                          
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.laboratory_yn                                            
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.laboratory_functional_yn                                 
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.staff_outreach_particip_yn
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.adult_tx_fees_yn                                         
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.child_tx_fees_yn                                         
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
#not_for_private_1.waste_disposal.note_waste_disposal                       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.waste_disposal.placenta_pit_yn                           
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.waste_disposal.sharps_separated_yn                       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment_functional.note_equipment_functional           
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment_functional.thermometer_funct_yn                
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment_functional.weighing_scale_funct_yn             
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment_functional.bp_machine_funct_yn                 
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment_functional.sterilizer_funct_yn                 
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment_functional.xray_funct_yn                       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment_functional.ultra_sound_funct_yn                
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment_functional.aspirator_funct_yn                  
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment_functional.extractor_funct_yn                  
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment_functional.forceps_funct_yn                    
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment_functional.antishock_funct_yn                  
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment_functional.oxygen_funct_yn                     
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment_functional.ambubag_funct_yn                    
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment_functional.neonatal_mask_funct_yn              
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment_functional.iv_kits_funct_yn                    
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment_functional.suction_funct_yn                    
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.note_medication_out_of_stock     
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.antibiotics_oral_stockout_yn     
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.antibiotics_musc_stockout_yn     
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.antibiotics_iv_stockout_yn       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.iv_fliud_stockout_yn             
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.ort_stockout_yn                  
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.uterotonics_stockout_yn          
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.antidiarrheal_stockout_yn        
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.antipyretics_stockout_yn         
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.act_stockout_yn                  
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.sulphadoxine_stockout_yn         
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.nevirapine_stockout_yn           
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.arvs_stockout_yn                 
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.azt_stockout_yn                  
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.tb_meds_stockout_yn              
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.sedatives_stockout_yn          
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.antihistamines_stockout_yn       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.anticonvulsants_stockout_yn      
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.oral_contacept_stockout_yn       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.inject_contacept_stockout_yn     
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.implants_stockout_yn      
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication_out_of_stock.iud_stockout_yn             
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.supplements_out_of_stock.note_supplements_out_of_stock  
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.supplements_out_of_stock.zinc_stockout_yn    
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.supplements_out_of_stock.iron_stockout_yn                
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.supplements_out_of_stock.folic_acid_stockout_yn          
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.supplements_out_of_stock.vitamin_a_stockout_yn          
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.immunization_out_of_stock.note_immunization_out_of_stock 
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.immunization_out_of_stock.bcg_immun_stockout_yn    
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.immunization_out_of_stock.opv_immun_stockout_yn          
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.immunization_out_of_stock.measles_immun_stockout_yn      
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.immunization_out_of_stock.dpt_immun_stockout_yn       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.immunization_out_of_stock.yfever_immun_stockout_yn     
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.immunization_out_of_stock.csm_immun_stockout_yn        
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.immunization_out_of_stock.hepb_immun_stockout_yn       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.immunization_out_of_stock.tetanus_immun_stockout_yn     
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.health_insurance.note_health_insurance       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.health_insurance.national_health_insurance             
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.health_insurance.community_health_insurance              
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.health_insurance.state_free_health_scheme                
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.health_insurance.other_health_insurance                                                                                            
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##facility_name                                                              
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##road_type_other_specify                                                    
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.power_sources.generator                                  
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.power_sources.solar                                      
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.power_sources.grid                                       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.power_sources.none                                       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.vaccine_storage_type.freezer                             
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.vaccine_storage_type.refrigerator                        
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.vaccine_storage_type.vaccine_carrier                     
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.vaccine_storage_type.cold_chain_box                      
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.vaccine_storage_type.other                               
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.vaccine_storage_type_other                               
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.cold_storage_system_type_other                           
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.water_sources.none                                       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.water_sources.borehole_tube_well                         
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.water_sources.protected_well                             
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.water_sources.unprotected_well                           
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.water_sources.protected_spring                           
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.water_sources.unprotected_spring                         
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.water_sources.rain_water                                 
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.water_sources.tap_outside                                
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.water_sources.tap_in_compound                            
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.water_sources.cart                                       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.water_sources.surface_water                              
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.water_sources.tanker_truck                               
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.facility_construction_responsible.federalgovernment      
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.facility_construction_responsible.stategovernment        
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.facility_construction_responsible.lga                    
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.facility_construction_responsible.localngocivilsociety   
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.facility_construction_responsible.internationaldevpartner
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.facility_construction_responsible.privatesector          
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.facility_construction_responsible.mdgconditionalgrants   
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.facility_construction_responsible.constituencyproject    
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.facility_construction_responsible.none               
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.facility_construction_responsible.dk                     
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.info_tech_available.landline                             
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.info_tech_available.mobile_facility                      
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.info_tech_available.mobile_staff                         
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.info_tech_available.credit                               
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.info_tech_available.computer                             
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.info_tech_available.internet                             
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.info_tech_available.printer                              
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_1.info_tech_available.none                                 
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.lab_tests.urine_testing                                  
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.lab_tests.pregnancy                                      
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.lab_tests.stool                                          
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.lab_tests.hemoglobin_testing                             
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.lab_tests.tb_microscopy                                  
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.lab_tests.hiv_testing                                    
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.lab_tests.cd4                                            
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.lab_tests.malaria_rdt                                    
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.lab_tests.malaria_microscopy                             
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.lab_tests.widal                                          
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.lab_tests.none                                           
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment.thermometer                                    
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment.scale                                          
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment.bp_machine                                     
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment.sterilizer                                     
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment.x_ray                                          
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment.ultrasound                                  
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment.aspirator                                      
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment.emoc_vacuum_extractor                          
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment.emoc_forceps                                   
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment.emoc_antishock_garment                         
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment.compr_oc_oxygen_tank                           
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment.ambubag                                        
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment.neonatal_mask                                  
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment.iv_kits                                       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment.compr_oc_suction_machine                       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.equipment.none                                           
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.supplies.syringes                                        
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.supplies.disposable_gloves                               
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.supplies.muac_tape                                       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.supplies.condoms                                         
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.supplies.insecticide_treated_bednets                     
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.supplies.insecticide_for_retreatment_bednets             
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.supplies.sterile_sputum_receptacles                      
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.supplies.needles_and_tubing                              
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.supplies.none                                            
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.antibiotic_oral                               
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.antibiotic_musc                               
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.antibiotic_iv                                 
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.iv_fluid                                      
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.ort                                           
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.uterotonics                                   
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.anti_diarrheals                               
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.anti_pyretics                                 
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.act                                           
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.sulphadoxine                                  
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.arvs                                          
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.nevirapine                                    
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.azt                                           
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.tb_medicines                                  
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.sedatives                                     
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.antihistamines                                
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.anticonvulsants                               
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.oral_contraceptives                           
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.injectable_contracept                         
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.implants                                      
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.iud                                           
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.medication.none                                          
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.supplements.zinc                                         
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.supplements.iron                                         
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.supplements.folic_acid                                   
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.supplements.vitamin_a                                    
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.supplements.none                                        
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.immunization.bcg_immunization                            
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.immunization.opv_immuization                            
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.immunization.measles_immun                            
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.immunization.dpt_immunization                            
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.immunization.yellow_fever_immun                          
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.immunization.csm_immunization                            
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.immunization.hepb_immunization                          
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.immunization.tetanus_immun                              
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.immunization.none                                   
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_adults.paid_services_routine_visit                
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_adults.paid_services_registration                 
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_adults.paid_services_routine_anc_visit            
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_adults.paid_services_anc_delivery                  
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_adults.paid_services_csection                      
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_adults.paid_services_medication                     
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_adults.paid_services_malaria_treatment              
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_adults.paid_services_contraceptives                
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_adults.paid_services_immunization                  
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_adults.paid_services_lab_testing                  
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_adults.paid_services_transport                    
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_adults.none                                      
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_children.ch_paid_services_routine_visit            
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_children.ch_paid_registration                       
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_children.ch_paid_services_medication                
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_children.ch_paid_services_transport                
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_children.ch_paid_malaria_treatment                
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_children.ch_paid__immunization                      
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_children.ch_paid_services_lab_testing              
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.fees_children.none                            
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           
##not_for_private_2.other_health_insurance_specify 
           ggplot(h, 
                  aes(x=facility_owner_manager.stategovernment, 
                      y=(..count..)/sum(..count..)*100)) +  
                        geom_bar() + 
                        ylab('Percent (%)')
           

           
