library(readxl)
library(dplyr)

# Load Data sets
Uganda_TwoTests <- "~/Superstudio things/jye_JHBHG_internship/Uganda/Data/Uganda_TwoTests.xlsx"

# Load Long and Wide
Long_File <- read_excel(path = Uganda_TwoTests, sheet = 1)
Wide_File <- read_excel(path = Uganda_TwoTests, sheet = 2)
# Load Flanker and Face
Flanker <- read_excel(path = Uganda_TwoTests, sheet = 4)
Face <- read_excel(path = Uganda_TwoTests, sheet = 3)

## Check 1: Verify that Wide file = merged Face and Flanker by column, extra columns at the end

# Merge dataframes
same_cols <- Reduce(intersect, list(names(Face), names(Flanker)))
Wide_Check <- merge(select(Face, -test), select(Flanker, -date, -test, -session), by = "subject")

names_df <- data.frame(names(Wide_Check), c(names(Wide_File)))
names(Wide_Check) <- names(Wide_File)

dplyr::all_equal(Wide_Check, Wide_File)
# -> Different types for column `date`: datetime<UTC> vs character.
# I just didn't change the date-time to date

## Check 2: Verify that Long = Flanker + Face alternating by row
# For simplicity, I will assume that if the first 12 Columns are accurate, the rest will be accurate

# Sort and check new long sheet
Long_Check <- rbind(Flanker[1:17], Face[1:17])
Long_Check <- Long_Check[gtools::mixedorder(Long_Check$subject),]

dplyr::all_equal(Long_Check, Long_File[1:17])
# -> Returns TRUE
