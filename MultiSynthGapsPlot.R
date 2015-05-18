
setMethod(f = "gaps.plot",
          signature = "MultiSynth",
          def = function(synth.res = input, dataprep.res = NA, Ylab = c("Title"),Xlab = c("Time"), Main = c("Gaps: Treated - Synthetic"), tr.intake = NA, Ylim = NA, Z.plot = FALSE){
            
            #prep additional lines 
            plot_prep <- MultiSynthGapPlotPrep(synth.res@preps[2:length(synth.res@preps)], synth.res@fits[2:length(synth.res@fits)] )
            
            Max_gap <- max(abs(synth.res@input$Y1plot - (synth.res@input$Y0plot %*% synth.res@fits[[1]]$solution.w))) 
            
            y_lim_gaps <- if(any(sapply(plot_prep, function(list){ max(abs(list$y.info)) > Max_gap})) ){max(sapply(plot_prep, function(list){ max(abs(list$y.info) )} ) ) }  else { Max_gap} 
            
            y_lim_final <- if(is.na(Ylim[1])){c(-1*(y_lim_gaps + .3*y_lim_gaps), y_lim_gaps + .3*y_lim_gaps)} else {Ylim}
            
            #plot the basic thing
            gaps.plot(synth.res@fits[[1]], synth.res@input, synth.res@treatment_time, Ylab = Ylab, Xlab= Xlab, Ylim = y_lim_final, Main = Main, Z.plot = Z.plot)
            
            #plot new lines
            MultiSynthPlotLines(plot_prep)  
            
            return(invisible())
          })
