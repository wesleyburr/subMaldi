## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(subMALDI)

## ----eval = FALSE-------------------------------------------------------------
#  my_spec1 <- readcsvSpec(spec_file = "~/my_spec/Sample1.csv", massCol = "m.z", intenseCol = "I")

## ----eval = FALSE-------------------------------------------------------------
#  readcsvDir(direct = "~/my_spec/", massCol = "m.z", intenseCol = "I", output = "~/my_spec/out/")
#  
#  load("~/my_spec/out/Sample1.rda")

## -----------------------------------------------------------------------------
data("Blank1")
head(Blank1)

## -----------------------------------------------------------------------------
spec_df <- createSpecDF(min_mz = 53.76, max_mz = 1100, res = 1e-04, dig = 4)

head(spec_df)

## -----------------------------------------------------------------------------
spec_df <- select(spec_df, "full_mz")
spec_df <- transform(spec_df, "Blank1" = 0, "Blank2" = 0, "Before1" = 0, "Before2" = 0)

head(spec_df)

## -----------------------------------------------------------------------------
data("Blank1")
data("Blank2")
data("Before1")
data("Before2")

## -----------------------------------------------------------------------------
spec_df <- mapSpectrum(dat = Blank1, massCol = "mass", intenseCol = "Intensity", 
            spec_df = spec_df, colName = "Blank1", thresh = 1e-04, dig = 4)
spec_df <- mapSpectrum(dat = Blank2, massCol = "mass", intenseCol = "Intensity", 
            spec_df = spec_df, colName = "Blank2", thresh = 1e-04, dig = 4)
spec_df <- mapSpectrum(dat = Before1, massCol = "mass", intenseCol = "Intensity", 
            spec_df = spec_df, colName = "Before1", thresh = 1e-04, dig = 4)
spec_df <- mapSpectrum(dat = Before2, massCol = "mass", intenseCol = "Intensity", 
            spec_df = spec_df, colName = "Before2", thresh = 1e-04, dig = 4)

head(spec_df)

## -----------------------------------------------------------------------------
spec <- rmveEmpty(spec_df)

head(spec)
nrow(spec)

## -----------------------------------------------------------------------------
avg_spec <- avgSpectra(spec, method = "mean", spec1 = "Blank1", spec2 = "Blank2")

head(avg_spec)

## -----------------------------------------------------------------------------
spec <- select(avg_spec, "full_mz", "Before1", "Before2", "Average")
sub_spec <- transform(spec, "Sub_Before1" = 0, "Sub_Before2" = 0)

## -----------------------------------------------------------------------------
sub_spec <- subSpectra(dat = sub_spec, Blank_Var = "Average", 
                       Sample = "Before1", Sub_Sample = "Sub_Before1")
sub_spec <- subSpectra(dat = sub_spec, Blank_Var = "Average", 
                       Sample = "Before2", Sub_Sample = "Sub_Before2")

head(sub_spec)

## -----------------------------------------------------------------------------
data("Master2")
norm_spec <- select(Master2, "full_mz", "Before1", "Before2")

head(norm_spec)

## ----warning = FALSE, fig.align = "center", fig.width = 6, fig.height = 3-----
plotSpectra(norm_spec,mass_dat = "full_mz", spec1 = "Before1", 
            spec2 = "Before2", lbl.fmt = "%3.2f")

## ----warning = FALSE, fig.show = "hold"---------------------------------------
custom_ex <- normSpectra(norm_spec, mass_dat = "full_mz", method = "custom", 
                         norm_mz = 255.23, spec1 = "Before1", spec2 = "Before2")

plotSpectra(norm_spec,mass_dat = "full_mz", spec1 = "Before1", 
            spec2 = "Before2", x_ticks = 250 , lbl.fmt = "%3.2f")
plotSpectra(custom_ex, mass_dat = "full_mz", spec1 = "Before1", spec2 = "Before2",
            x_ticks = 250, colour1 = "lightgreen",
            colour2 = "cornflowerblue", lbl.fmt = "%3.2f")

## ----warning = FALSE, fig.show = "hold"---------------------------------------
max_ex <- normSpectra(norm_spec, mass_dat = "full_mz", method = "max", 
                      spec1 = "Before1", spec2 = "Before2")

plotSpectra(norm_spec,mass_dat = "full_mz", spec1 = "Before1", 
            spec2 = "Before2", x_ticks = 250, lbl.fmt = "%3.2f")
plotSpectra(max_ex, mass_dat = "full_mz", spec1 = "Before1", spec2 = "Before2", 
            x_ticks = 250, colour1 = "lightgreen",
            colour2 = "cornflowerblue", lbl.fmt = "%3.2f")

## ----warning = FALSE, fig.show = "hold"---------------------------------------
maxset_ex <- normSpectra(norm_spec, mass_dat = "full_mz", method = "max_set", 
                         spec1 = "Before1", spec2 = "Before2")

plotSpectra(norm_spec,mass_dat = "full_mz", spec1 = "Before1", 
            spec2 = "Before2", x_ticks = 250, lbl.fmt = "%3.2f")
plotSpectra(maxset_ex, mass_dat = "full_mz", spec1 = "Before1", spec2 = "Before2", 
            x_ticks = 250, intensity_scale = "fixed", colour1 = "lightgreen",
            colour2 = "cornflowerblue", lbl.fmt = "%3.2f")

## ----warning = FALSE, fig.show = "hold"---------------------------------------
RMS_ex <- normSpectra(norm_spec, mass_dat = "full_mz", method = "RMS", spec1 = "Before1", 
                      spec2 = "Before2")

plotSpectra(norm_spec,mass_dat = "full_mz", spec1 = "Before1", 
            spec2 = "Before2", x_ticks = 250, lbl.fmt = "%3.2f")
plotSpectra(RMS_ex, mass_dat = "full_mz", spec1 = "Before1", spec2 = "Before2", 
            x_ticks = 250, colour1 = "lightgreen",
            colour2 = "cornflowerblue", lbl.fmt = "%3.2f")

## ----warning = FALSE, fig.show = "hold"---------------------------------------
TIC_ex <- normSpectra(norm_spec, mass_dat= "full_mz", method = "TIC", spec1 = "Before1",
                      spec2 = "Before2")

plotSpectra(norm_spec,mass_dat = "full_mz", spec1 = "Before1", 
            spec2 = "Before2", x_ticks = 250, lbl.fmt = "%3.2f")
plotSpectra(TIC_ex, mass_dat = "full_mz", spec1 = "Before1", spec2 = "Before2",
            x_ticks = 250, colour1 = "lightgreen",
            colour2 = "cornflowerblue", lbl.fmt = "%3.2f")

## ----warning = FALSE, fig.show = "hold"---------------------------------------
rel_TIC_ex <- normSpectra(norm_spec, mass_dat = "full_mz", method = "rel_TIC", spec1 = "Before1",spec2 = "Before2")

plotSpectra(norm_spec,mass_dat = "full_mz", spec1 = "Before1", 
            spec2 = "Before2", x_ticks = 250, lbl.fmt = "%3.2f")
plotSpectra(rel_TIC_ex, mass_dat = "full_mz", spec1 = "Before1", 
            spec2 = "Before2",x_ticks = 250, colour1 = "lightgreen",
            colour2 = "cornflowerblue", lbl.fmt = "%3.2f")

## -----------------------------------------------------------------------------
head(rel_TIC_ex)

## -----------------------------------------------------------------------------
sum(rel_TIC_ex$Before1)
sum(rel_TIC_ex$Before2)
sum(rel_TIC_ex[,-1])

## ----warning = FALSE, fig.show = "hold"---------------------------------------

median_ex <- normSpectra(dat = norm_spec, mass_dat = "full_mz", method = "median", 
                         spec1 = "Before1", spec2 = "Before2")

plotSpectra(dat = norm_spec, mass_dat = "full_mz", spec1 = "Before1", 
            spec2 = "Before2", x_ticks = 250, lbl.fmt = "%3.2f")
plotSpectra(dat = median_ex, mass_dat = "full_mz", spec1 = "Before1", 
            spec2 = "Before2",x_ticks = 250, colour1 = "lightgreen", 
            colour2 = "cornflowerblue", lbl.fmt = "%3.2f")

## ----warning = FALSE, fig.width = 6, fig.height = 3, fig.align = "center"-----
plotSpectra(norm_spec, mass_dat = "full_mz", spec1 = "Before1", 
            spec2 = "Before2", x_ticks = 100, min_mz = 750, max_mz = 1100,
            lbl.fmt = "%3.2f")

## ----warning = FALSE, fig.show = "hold"---------------------------------------
stdev_ex <- normSpectra(norm_spec, mass_dat = "full_mz", method = "stdev", 
                        lower = 900, upper = 1000, spec1 = "Before1", 
                        spec2 = "Before2")

plotSpectra(norm_spec,mass_dat = "full_mz", spec1 = "Before1", 
            spec2 = "Before2", x_ticks = 250, lbl.fmt = "%3.2f")
plotSpectra(stdev_ex, mass_dat = "full_mz", spec1 = "Before1", 
            spec2 = "Before2", x_ticks = 250, colour1 = "lightgreen",
            colour2 = "cornflowerblue", lbl.fmt = "%3.2f")

## ----warning = FALSE, fig.show = "hold"---------------------------------------
quantile_ex <- normSpectra(norm_spec, mass_dat = "full_mz", method = "quantile", 
                           spec1 = "Before1", spec2 = "Before2")

plotSpectra(norm_spec,mass_dat = "full_mz", spec1 = "Before1", 
            spec2 = "Before2", x_ticks = 250, lbl.fmt = "%3.2f")
plotSpectra(quantile_ex, mass_dat = "full_mz", spec1 = "Before1", 
            spec2 = "Before2", x_ticks = 250, colour1 = "lightgreen",
            colour2 = "cornflowerblue", lbl.fmt = "%3.2f")

## ----warning = FALSE, fig.width = 6, fig.height = 3, fig.align = "center"-----
data("Blank1")
plotSpectrum(dat = Blank1, mass_dat = Blank1$mass, intensity_dat = Blank1$Intensity, 
             lbls = TRUE, span = 9)

## ----warning = FALSE, fig.width = 6, fig.height = 3, fig.align = "center"-----
data("Master")
plotSpectrum(dat = Master, mass_dat = Master$full_mz, intensity_dat = Master$After2, 
             colour = "black", min_mz = 100, max_mz = 500)

## -----------------------------------------------------------------------------
data("Master2")

## ----warning = FALSE, fig.width = 6, fig.height = 3, fig.align = "center"-----
plotSpectrum(Master2, mass_dat = Master2$full_mz, intensity_dat = Master2$Before1, lbls = TRUE, lbl.fmt = "%3.2f")

## ----warning = FALSE, fig.width = 6, fig.height = 6, fig.align = "center"-----
plotSpectra(dat = Master, mass_dat = "full_mz", spec1 = "Before1", spec2 = "Before2",
             spec3 = "After1", spec4 = "After2", lbls = TRUE, span = 15)

## ----warning = FALSE, fig.width = 6, fig.height = 6, fig.align = "center"-----
plotgridSpectra(Master, mass_dat = "full_mz", spec1 = "Blank1", spec2 = "Blank2",
                spec3 = "Before1", spec4 = "Before2", x_ticks = 500, lbls = TRUE,
                span = 11)

## ----warning = FALSE, fig.width = 6, fig.height = 8, fig.align = "center"-----
plotgridSpectra(Master, mass_dat = "full_mz", spec1 = "Blank1", spec2 = "Blank2",
                spec3 = "Before1", spec4 = "Before2", spec5 = "After1", spec6 = "After2", 
                x_ticks = 500, max_mz = 500, lbls = TRUE, span = 11)

## ----warning = FALSE, fig.width = 6, fig.height = 4, fig.align = "center"-----
plotgridSpectra(Master, mass_dat = "full_mz", spec1 = "Blank1", spec2 = "Blank2",
                spec3 = "Before1", spec4 = "Before2", spec5 = "After1", spec6 = "After2", 
                x_ticks = 500, max_mz = 500, lbls = TRUE, columns = 3, span = 11)

