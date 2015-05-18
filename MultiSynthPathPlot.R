setMethod(f = "path.plot",
          signature = "MultiSynth",
          def = function(synth.res = input, dataprep.res = NA, tr.intake = NA, Ylab = c("Y Axis"), Xlab = c("Time"), Ylim = NA, Legend = c("Treated", "Synthetic", "Alternative Synthetics"), Legend.position = c("topleft"), Main = NA, Z.plot = FALSE){
            
            #plot the basic thing
            path.plot(synth.res@fits[[1]], synth.res@input, synth.res@treatment_time, Ylab = Ylab, Xlab= Xlab, Ylim = Ylim, Legend = NA, Legend.position = Legend.position, Main = Main, Z.plot = Z.plot)
            
            #prep the additional lines
            plot_prep <- MultiSynthPathPlotPrep(synth.res@preps[2:length(synth.res@preps)], synth.res@fits[2:length(synth.res@fits)] )
            
            #plot new lines
            MultiSynthPlotLines(plot_prep) 
            
            if (sum(is.na(Legend)) == 0) {  
            legend(Legend.position, legend = Legend, lty = c(1,2,1), col = c("black", "black", "grey"), lwd = c(2, 2, 2), cex = 6/7)
            }
            
            return(invisible())
          } #end function
) #end setMethod
