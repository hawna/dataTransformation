#Load libraries
library (ggplot2)
library (gcookbook)
library (tidyr)
library (reshape2)
library (dplyr)

#Set Directory
setwd("/Users/file")

##Take file from SIS system
mapimport <- read.csv("file.csv", header = TRUE)

dim (mapimport)
head(mapimport)

#Long to Wide using reshape2
data_melt <- melt(mapimport,
                  id.vars=c(
                    "SchoolStateCode",
                    "SchoolName",
                    "PreviousInstructorID",
                    "InstructorID",
                    "InstructorStateID",
                    "InstructorLastName",
                    "InstructorFirstName",
                    "InstructorMiddleInitial",
                    "UserName",
                    "EmailAddress",
                    "ClassTitle",
                    "ClassTitle_Section",
                    "PreviousStudentID",
                    "StudentNumber",
                    "StudentStateID",
                    "StudentLastName",
                    "StudentFirstName",
                    "StudentMiddleInitial",
                    "StudentBirthDate",
                    "StudentGender",
                    "StudentGradeLevel",
                    "StudentEthnicityRace",
                    "StudentUserName",
                    "StudentEmail",
                    "ClassSection",
                    "CourseID",
                    "ClassID",
                    "ClassCredits",
                    "MaxGradeLevelID",
                    "TakeAttendance",
                    "InstructorCodes"),
                  measure.vars=c(
                    "InstructorCode1",
                    "InstructorCode2",
                    "InstructorCode3",
                    "InstructorCode4"),
                  variable.name="InstructorNumber",
                  value.name = "teacherID"
)

##Sort 
data_melt <- data_melt[order(data_melt$"ClassTitle_Section",
                             data_melt$"StudentNumber", 
                             data_melt$"teacherID"), ]

##Match columns to specs for import
MAP_import <- data_melt[, c("SchoolStateCode",
                            #"SchoolName",
                            "PreviousInstructorID",
                            "teacherID",
                            "InstructorStateID",
                            "InstructorLastName",
                            "InstructorFirstName",
                            "InstructorMiddleInitial",
                            #"UserName",
                            #"EmailAddress",
                            "ClassTitle_Section",
                            "PreviousStudentID",
                            "StudentNumber",
                            "StudentStateID",
                            "StudentLastName",
                            "StudentFirstName",
                            "StudentMiddleInitial",
                            "StudentBirthDate",
                            "StudentGender",
                            "StudentGradeLevel",
                            "StudentEthnicityRace"
                            #"StudentUserName",
                            #"StudentEmail"
)]

##Fix form of date to import specs
"StudentBirthDate" <- as.Date("StudentBirthDate", "%m/%d/%Y")

##Possible ways to fix ethnicity wording
#as.character(MAP_import, "StudentEthnicityRace")
#x <- c("American Indian or Alaska Native")
#gsub("Alaska", "Alaskan", x)

dim (MAP_import)

MAP_import <- filter(MAP_import, teacherID != "NA")

dim (MAP_import)

##Write to csv for import
write.csv(MAP_import, file = "MAP_Import.csv", row.names = FALSE)


##Same idea in tidyr
###data_long <- gather(mapimport, teacherNum, TeacherID, TeacherID1:TeacherID3, factor_key = TRUE)
###data_long <- data_long[order(data_long$ClassID, data_long$Student, data_long$TeacherID), ]
###data_long