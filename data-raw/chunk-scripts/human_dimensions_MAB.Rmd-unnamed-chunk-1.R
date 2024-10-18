
dat<- data.frame(XSurvey= c("Fall BTS", "Spring BTS", "EcoMon", "Scallop",
                            "Shellfish(Clams)", "Right Whale (Air)", 
                            "Marine Mammal/Turtle (Ship/Air)",
                            "Altantic Shark (Bottom Long-Line",
                            "GOM Bottom Long-Line", "GOM Shrimp Survey", 
                            "Atlantic Shark COASTPAN"), 
                 X1 = c("Started", "Started", "No",
                                                    "Started", "No", "Inital", 
                                                    "No", "No","No", "No","No"), 
                 X2 = c("Inital", "Initial", "No", "Initial", 
                                            "No", "Initial","No", "No","No", "No",
                                            "No" ), 
                 X3 = c( "No", "No","No",
                        "No","No",  "Initial", "No", "No","No", "No","No"), 
                 X4 = c("No", "No","No", "No","No","No", 
                                         "No","No", "No","No","No"), 
                 X5 = c("No", "No","No", "No","No","No", 
                                         "No","No", "No","No","No"),
                 X6 = c("Initial", "Initial","No", "No","No","No", 
                                         "No","No", "No","No","No"))

column_names <- c("Survey","1.Evaluate designs & Impacts","2.Design New Methods","3.Calibrate New/Existing Surveys","4.Bridge Solutions","5.Conduct New Surveys","6.Comms & Data")
#DT::datatable(dat, colnames = column_names)

kable(dat, col.names = column_names)
