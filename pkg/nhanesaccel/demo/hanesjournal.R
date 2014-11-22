# Run nhanesaccel code from paper in The R Journal

# Process NHANES 2003-2006 data using default settings
nhanes1 <- nhanes.accel.process()

# Examine summary data for first 5 participants
nhanes1[1:5, ]

readline("Press <Enter> to continue")

# Process NHANES 2003-2006 data, requiring at least one valid weekday and weekend day
nhanes2 <- nhanes.accel.process(valid.week.days = 1, valid.weekend.days = 1,
                                weekday.weekend = TRUE)

# Get dimension and variable names for nhanes2
dim(nhanes2)
names(nhanes2)

readline("Press <Enter> to continue")

# Load demographics data and merge with nhanes2
data(dem)
nhanes2 <- merge(x = nhanes2, y = dem)

readline("Press <Enter> to continue")

# Calculate percent difference between weekday and weekend CPM for each participant
nhanes2$cpm_diff <- (nhanes2$wk_cpm - nhanes2$we_cpm) /
                    ((nhanes2$wk_cpm + nhanes2$we_cpm) / 2) * 100

readline("Press <Enter> to continue")

# Create survey object called hanes
hanes <- svydesign(id = ~sdmvpsu, strata = ~sdmvstra, weight = ~wtmec4yr_adj,
                   data = nhanes2, nest = TRUE)

readline("Press <Enter> to continue")

# Calculate mean (SE) and 95% CI's for cpm_diff for ages 6 to 18 years
mean.diff <- svyby(~cpm_diff, by = ~ridageyr, design = subset(hanes, ridageyr <= 18),
                   FUN = svymean, na.rm = TRUE)
ci <- confint(mean.diff)

readline("Press <Enter> to continue")

# Plot means and CI's for cpm_diff by age
plot(x = 6:18, y = mean.diff[, 2], main = "CPM on Weekdays vs. Weekends",
     ylim = c(-30, 30), pch = 19, ylab = "Perc. diff. (mean +/- 95% CI)",
     xlab = "Age (years)", cex = 0.8, cex.axis = 0.85, cex.main = 1.5,
     cex.lab = 1.1, xaxt = "n")
axis(side = 1, at = 6:18, cex.axis = 0.85)
segments(x0 = 6:18, y0 = ci[, 1], x1 = 6:18, y1 = ci[, 2], lwd = 1.3)
abline(h = 0, lty = 2)

readline("Press <Enter> to continue")

# Process NHANES 2003-2006 data with four non-default inputs
nhanes3 <- nhanes.accel.process(brevity = 3, valid.days = 4, nonwear.window = 90,
                                weekday.weekend = TRUE)

readline("Press <Enter> to continue")

# Specify count cut-points for moderate and vigorous intensity in youth
youthmod <- c(1400, 1515, 1638, 1770, 1910, 2059, 2220, 2393, 2580, 2781, 3000, 3239)
youthvig <- c(3758, 3947, 4147, 4360, 4588, 4832, 5094, 5375, 5679, 6007, 6363, 6751)

# Process NHANES 2003-2006 data using NCI's methods
nci1 <- nhanes.accel.process(waves = 3, brevity = 2, valid.days = 4,
                             youth.mod.cuts = youthmod, youth.vig.cuts = youthvig,
                             cpm.nci = TRUE, days.distinct = TRUE, nonwear.tol = 2,
                             nonwear.tol.upper = 100, nonwear.nci = TRUE,
                             weartime.maximum = 1440, active.bout.tol = 2,
                             active.bout.nci = TRUE, artifact.thresh = 32767,
                             artifact.action = 3)

readline("Press <Enter> to continue")

# Repeat, but use nci.methods input for convenience
nci2 <- nhanes.accel.process(waves = 3, brevity = 2, nci.methods = TRUE)

readline("Press <Enter> to continue")

# Verify that nci1 and nci2 are equivalent
all(nci1 == nci2, na.rm = TRUE)