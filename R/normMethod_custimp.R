# -----------------------------------------------------------------------
# Last Updated: July 30, 2020
# Author: Kristen Yeh
# Title: subMALDI: Normalization Method - Custom m/z, imprecise
# -----------------------------------------------------------------------


# --------------------------------------
# METHOD: CUSTOM M/Z VALUE, NOT PRECISE
# --------------------------------------


.normMethod_custimp <- function(dat, mass_dat, norm_mz, spec1, 
                                spec2 = NULL, spec3 = NULL, 
                                spec4 = NULL, spec5 = NULL, 
                                spec6 = NULL, showHI = FALSE){
  if(showHI == FALSE){
    if(is.null(norm_mz)){
      stop('Please select a m/z value. norm_mz is NULL.')
    } else{
      
      if(is.null(spec6)){
        
        if(is.null(spec5)){
          
          if(is.null(spec4)){
            
            if(is.null(spec3)){
              
              if(is.null(spec2)){
                
                # One spectrum
                mass <- dat[[mass_dat]]
                intense <- dat[[spec1]]
                
                i <- which(startsWith(as.character(mass), norm_mz))
                custom_int <- intense[i]
                
                if(length(custom_int) > 1){
                  stop("More than one peak per spectra for the given m/z value. 
                     Please be more precise.")
                } else{
                  
                  if(custom_int > 0){
                    dat <- select(dat, all_of(mass_dat))
                    norm <- .norm_custom(y = intense, custom_y = custom_int)
                    norm[norm > 1] <- 1 
                    dat[spec1] <- norm
                    return(dat)}
                  else{stop('The selected maximum intensity is 0.')}
                }}
              # Two spectra
              else {
                mass <- dat[[mass_dat]]
                intense1 <- dat[[spec1]]
                intense2 <- dat[[spec2]]
                
                i <- which(startsWith(as.character(mass), norm_mz))
                custom_int1 <- intense1[i]
                custom_int1 <- custom_int1[which(custom_int1 != 0)]
                custom_int2 <- intense2[i]
                custom_int2 <- custom_int2[which(custom_int2 != 0)]
                
                if(length(custom_int1) > 1 | length(custom_int2) > 1){
                  stop('More than one peak per spectra for the given m/z value. 
                     Please be more precise.')
                } else if(length(custom_int1) < 1 | length(custom_int2) < 1){  
                  stop('The selected maximum intensity is 0 in some spectra.')
                } else{
                  dat <- select(dat, all_of(mass_dat))
                  norm1 <- .norm_custom(y = intense1, custom_y = custom_int1)
                  norm2 <- .norm_custom(y = intense2, custom_y = custom_int2)
                  
                  norm1[norm1 > 1] <- 1 
                  norm2[norm2 > 1] <- 1 
                  dat[spec1] <- norm1
                  dat[spec2] <- norm2
                  return(dat)} 
              }}
            
            # Three spectra
            else{
              mass <- dat[[mass_dat]]
              intense1 <- dat[[spec1]]
              intense2 <- dat[[spec2]]
              intense3 <- dat[[spec3]]
              
              i <- which(startsWith(as.character(mass), norm_mz))
              custom_int1 <- intense1[i]
              custom_int1 <- custom_int1[which(custom_int1 != 0)]
              custom_int2 <- intense2[i]
              custom_int2 <- custom_int2[which(custom_int2 != 0)]
              custom_int3 <- intense3[i]
              custom_int3 <- custom_int3[which(custom_int3 != 0)]
              
              if(length(custom_int1) > 1 | 
                 length(custom_int2) > 1 | 
                 length(custom_int3) > 1){
                stop('More than one peak per spectra for the given m/z value. 
                     Please be more precise.')
              } else if(length(custom_int1) < 1 | 
                        length(custom_int2) < 1 | 
                        length(custom_int3) < 1){  
                stop('The selected maximum intensity is 0 in some spectra.')
              } else{
                
                dat <- select(dat, all_of(mass_dat))
                norm1 <- .norm_custom(y = intense1, custom_y = custom_int1)
                norm2 <- .norm_custom(y = intense2, custom_y = custom_int2)
                norm3 <- .norm_custom(y = intense3, custom_y = custom_int3)
                
                norm1[norm1 > 1] <- 1 
                norm2[norm2 > 1] <- 1 
                norm3[norm3 > 1] <- 1 
                dat[spec1] <- norm1
                dat[spec2] <- norm2
                dat[spec3] <- norm3
                return(dat)} 
            }}
          
          # Four spectra
          else{
            mass <- dat[[mass_dat]]
            intense1 <- dat[[spec1]]
            intense2 <- dat[[spec2]]
            intense3 <- dat[[spec3]]
            intense4 <- dat[[spec4]]
            
            i <- which(startsWith(as.character(mass), norm_mz))
            custom_int1 <- intense1[i]
            custom_int1 <- custom_int1[which(custom_int1 != 0)]
            custom_int2 <- intense2[i]
            custom_int2 <- custom_int2[which(custom_int2 != 0)]
            custom_int3 <- intense3[i]
            custom_int3 <- custom_int3[which(custom_int3 != 0)]
            custom_int4 <- intense4[i]
            custom_int4 <- custom_int4[which(custom_int4 != 0)]
            
            if(length(custom_int1) > 1 | 
               length(custom_int2) > 1 | 
               length(custom_int3) > 1 |
               length(custom_int4) > 1){
              stop('More than one peak per spectra for the given m/z value. 
                     Please be more precise.')
            } else if(length(custom_int1) < 1 | 
                      length(custom_int2) < 1 | 
                      length(custom_int3) < 1 |
                      length(custom_int4) < 1){
              stop('The selected maximum intensity is 0 in some spectra.')
            } else{
              dat <- select(dat, all_of(mass_dat))
              norm1 <- .norm_custom(y = intense1, custom_y = custom_int1)
              norm2 <- .norm_custom(y = intense2, custom_y = custom_int2)
              norm3 <- .norm_custom(y = intense3, custom_y = custom_int3)
              norm4 <- .norm_custom(y = intense4, custom_y = custom_int4)
              
              norm1[norm1 > 1] <- 1 
              norm2[norm2 > 1] <- 1 
              norm3[norm3 > 1] <- 1 
              norm4[norm4 > 1] <- 1
              dat[spec1] <- norm1
              dat[spec2] <- norm2
              dat[spec3] <- norm3
              dat[spec4] <- norm4
              return(dat)}
          }}
        
        # Five spectra
        else{
          mass <- dat[[mass_dat]]
          intense1 <- dat[[spec1]]
          intense2 <- dat[[spec2]]
          intense3 <- dat[[spec3]]
          intense4 <- dat[[spec4]]
          intense5 <- dat[[spec5]]
          
          i <- which(startsWith(as.character(mass), norm_mz))
          custom_int1 <- intense1[i]
          custom_int1 <- custom_int1[which(custom_int1 != 0)]
          custom_int2 <- intense2[i]
          custom_int2 <- custom_int2[which(custom_int2 != 0)]
          custom_int3 <- intense3[i]
          custom_int3 <- custom_int3[which(custom_int3 != 0)]
          custom_int4 <- intense4[i]
          custom_int4 <- custom_int4[which(custom_int4 != 0)]
          custom_int5 <- intense5[i]
          custom_int5 <- custom_int5[which(custom_int5 != 0)]
          
          if(length(custom_int1) > 1 | 
             length(custom_int2) > 1 | 
             length(custom_int3) > 1 |
             length(custom_int4) > 1 |
             length(custom_int5) > 1 ){
            stop('More than one peak per spectra for the given m/z value. 
                     Please be more precise.')
          } else if(length(custom_int1) < 1 | 
                    length(custom_int2) < 1 | 
                    length(custom_int3) < 1 |
                    length(custom_int4) < 1 |
                    length(custom_int5) < 1 ){
            stop('The selected maximum intensity is 0 in some spectra.')
          } else{
            dat <- select(dat, all_of(mass_dat))
            norm1 <- .norm_custom(y = intense1, custom_y = custom_int1)
            norm2 <- .norm_custom(y = intense2, custom_y = custom_int2)
            norm3 <- .norm_custom(y = intense3, custom_y = custom_int3)
            norm4 <- .norm_custom(y = intense4, custom_y = custom_int4)
            norm5 <- .norm_custom(y = intense5, custom_y = custom_int5)
            
            norm1[norm1 > 1] <- 1 
            norm2[norm2 > 1] <- 1 
            norm3[norm3 > 1] <- 1 
            norm4[norm4 > 1] <- 1
            norm5[norm5 > 1] <- 1
            dat[spec1] <- norm1
            dat[spec2] <- norm2
            dat[spec3] <- norm3
            dat[spec4] <- norm4
            dat[spec5] <- norm5
            return(dat)}
        }}
      
      # Six spectra
      else{
        mass <- dat[[mass_dat]]
        intense1 <- dat[[spec1]]
        intense2 <- dat[[spec2]]
        intense3 <- dat[[spec3]]
        intense4 <- dat[[spec4]]
        intense5 <- dat[[spec5]]
        intense6 <- dat[[spec6]]
        
        i <- which(startsWith(as.character(mass), norm_mz))
        custom_int1 <- intense1[i]
        custom_int1 <- custom_int1[which(custom_int1 != 0)]
        custom_int2 <- intense2[i]
        custom_int2 <- custom_int2[which(custom_int2 != 0)]
        custom_int3 <- intense3[i]
        custom_int3 <- custom_int3[which(custom_int3 != 0)]
        custom_int4 <- intense4[i]
        custom_int4 <- custom_int4[which(custom_int4 != 0)]
        custom_int5 <- intense5[i]
        custom_int5 <- custom_int5[which(custom_int5 != 0)]
        custom_int6 <- intense6[i]
        custom_int6 <- custom_int6[which(custom_int6 != 0)]
        
        if(length(custom_int1) > 1 | 
           length(custom_int2) > 1 | 
           length(custom_int3) > 1 |
           length(custom_int4) > 1 |
           length(custom_int5) > 1 |
           length(custom_int6) > 1){
          stop('More than one peak per spectra for the given m/z value. 
                     Please be more precise.')
        } else if(length(custom_int1) < 1 | 
                  length(custom_int2) < 1 | 
                  length(custom_int3) < 1 |
                  length(custom_int4) < 1 |
                  length(custom_int5) < 1 |
                  length(custom_int6) < 1){  
          'The selected maximum intensity is 0 in some spectra.'
        } else{
          dat <- select(dat, all_of(mass_dat))
          norm1 <- .norm_custom(y = intense1, custom_y = custom_int1)
          norm2 <- .norm_custom(y = intense2, custom_y = custom_int2)
          norm3 <- .norm_custom(y = intense3, custom_y = custom_int3)
          norm4 <- .norm_custom(y = intense4, custom_y = custom_int4)
          norm5 <- .norm_custom(y = intense5, custom_y = custom_int5)
          norm6 <- .norm_custom(y = intense6, custom_y = custom_int6)
          
          norm1[norm1 > 1] <- 1 
          norm2[norm2 > 1] <- 1 
          norm3[norm3 > 1] <- 1 
          norm4[norm4 > 1] <- 1
          norm5[norm5 > 1] <- 1
          norm6[norm6 > 1] <- 1
          dat[spec1] <- norm1
          dat[spec2] <- norm2
          dat[spec3] <- norm3
          dat[spec4] <- norm4
          dat[spec5] <- norm5
          dat[spec6] <- norm6
          return(dat)}
      }
    }
  } else{
    
    if(is.null(norm_mz)){
      stop('Please select a m/z value. norm_mz is NULL.')
    } else{
      
      if(is.null(spec6)){
        
        if(is.null(spec5)){
          
          if(is.null(spec4)){
            
            if(is.null(spec3)){
              
              if(is.null(spec2)){
                
                # One spectrum
                mass <- dat[[mass_dat]]
                intense <- dat[[spec1]]
                
                i <- which(startsWith(as.character(mass), norm_mz))
                custom_int <- intense[i]
                
                if(length(custom_int) > 1){
                  stop('More than one peak per spectra for the given m/z value. 
                     Please be more precise.')
                } else{
                  if(custom_int > 0){
                    dat <- select(dat, all_of(mass_dat))
                    norm <- .norm_custom(y = intense, custom_y = custom_int)
                    dat[spec1] <- norm
                    return(dat)}
                  else{stop('The selected maximum intensity is 0.')}
                }} 
              
              # Two spectra
              else {
                mass <- dat[[mass_dat]]
                intense1 <- dat[[spec1]]
                intense2 <- dat[[spec2]]
                
                i <- which(startsWith(as.character(mass), norm_mz))
                custom_int1 <- intense1[i]
                custom_int1 <- custom_int1[which(custom_int1 != 0)]
                custom_int2 <- intense2[i]
                custom_int2 <- custom_int2[which(custom_int2 != 0)]
                
                if(length(custom_int1) > 1 | length(custom_int2) > 1){
                  stop('More than one peak per spectra for the given m/z value. 
                     Please be more precise.')
                } else if(length(custom_int1) < 1 | length(custom_int2) < 1){  
                  stop('The selected maximum intensity is 0 in some spectra.')
                } else{
                  dat <- select(dat, all_of(mass_dat))
                  norm1 <- .norm_custom(y = intense1, custom_y = custom_int1)
                  norm2 <- .norm_custom(y = intense2, custom_y = custom_int2)
                  
                  dat[spec1] <- norm1
                  dat[spec2] <- norm2
                  return(dat)} 
              }}
            
            # Three spectra
            else{
              mass <- dat[[mass_dat]]
              intense1 <- dat[[spec1]]
              intense2 <- dat[[spec2]]
              intense3 <- dat[[spec3]]
              
              i <- which(startsWith(as.character(mass), norm_mz))
              custom_int1 <- intense1[i]
              custom_int1 <- custom_int1[which(custom_int1 != 0)]
              custom_int2 <- intense2[i]
              custom_int2 <- custom_int2[which(custom_int2 != 0)]
              custom_int3 <- intense3[i]
              custom_int3 <- custom_int3[which(custom_int3 != 0)]
              
              if(length(custom_int1) > 1 | 
                 length(custom_int2) > 1 | 
                 length(custom_int3) > 1){
                stop('More than one peak per spectra for the given m/z value. 
                     Please be more precise.')
              } else if(length(custom_int1) < 1 | 
                        length(custom_int2) < 1 | 
                        length(custom_int3) < 1){  
                stop('The selected maximum intensity is 0 in some spectra.')
              } else{
                dat <- select(dat, all_of(mass_dat))
                norm1 <- .norm_custom(y = intense1, custom_y = custom_int1)
                norm2 <- .norm_custom(y = intense2, custom_y = custom_int2)
                norm3 <- .norm_custom(y = intense3, custom_y = custom_int3)
                
                dat[spec1] <- norm1
                dat[spec2] <- norm2
                dat[spec3] <- norm3
                return(dat)} 
            }}
          
          # Four spectra
          else{
            mass <- dat[[mass_dat]]
            intense1 <- dat[[spec1]]
            intense2 <- dat[[spec2]]
            intense3 <- dat[[spec3]]
            intense4 <- dat[[spec4]]
            
            i <- which(startsWith(as.character(mass), norm_mz))
            custom_int1 <- intense1[i]
            custom_int1 <- custom_int1[which(custom_int1 != 0)]
            custom_int2 <- intense2[i]
            custom_int2 <- custom_int2[which(custom_int2 != 0)]
            custom_int3 <- intense3[i]
            custom_int3 <- custom_int3[which(custom_int3 != 0)]
            custom_int4 <- intense4[i]
            custom_int4 <- custom_int4[which(custom_int4 != 0)]
            
            if(length(custom_int1) > 1 | 
               length(custom_int2) > 1 | 
               length(custom_int3) > 1 |
               length(custom_int4) > 1){
              stop('More than one peak per spectra for the given m/z value. 
                     Please be more precise.')
            } else if(length(custom_int1) < 1 | 
                      length(custom_int2) < 1 | 
                      length(custom_int3) < 1 |
                      length(custom_int4) < 1){
              stop('The selected maximum intensity is 0 in some spectra.')
            } else{
              dat <- select(dat, all_of(mass_dat))
              norm1 <- .norm_custom(y = intense1, custom_y = custom_int1)
              norm2 <- .norm_custom(y = intense2, custom_y = custom_int2)
              norm3 <- .norm_custom(y = intense3, custom_y = custom_int3)
              norm4 <- .norm_custom(y = intense4, custom_y = custom_int4)
              
              dat[spec1] <- norm1
              dat[spec2] <- norm2
              dat[spec3] <- norm3
              dat[spec4] <- norm4
              return(dat)}
          }}
        
        # Five spectra
        else{
          mass <- dat[[mass_dat]]
          intense1 <- dat[[spec1]]
          intense2 <- dat[[spec2]]
          intense3 <- dat[[spec3]]
          intense4 <- dat[[spec4]]
          intense5 <- dat[[spec5]]
          
          i <- which(startsWith(as.character(mass), norm_mz))
          custom_int1 <- intense1[i]
          custom_int1 <- custom_int1[which(custom_int1 != 0)]
          custom_int2 <- intense2[i]
          custom_int2 <- custom_int2[which(custom_int2 != 0)]
          custom_int3 <- intense3[i]
          custom_int3 <- custom_int3[which(custom_int3 != 0)]
          custom_int4 <- intense4[i]
          custom_int4 <- custom_int4[which(custom_int4 != 0)]
          custom_int5 <- intense5[i]
          custom_int5 <- custom_int5[which(custom_int5 != 0)]
          
          if(length(custom_int1) > 1 | 
             length(custom_int2) > 1 | 
             length(custom_int3) > 1 |
             length(custom_int4) > 1 |
             length(custom_int5) > 1 ){
            stop('More than one peak per spectra for the given m/z value. 
                     Please be more precise.')
          } else if(length(custom_int1) < 1 | 
                    length(custom_int2) < 1 | 
                    length(custom_int3) < 1 |
                    length(custom_int4) < 1 |
                    length(custom_int5) < 1 ){
            stop('The selected maximum intensity is 0 in some spectra.')
          } else{
            dat <- select(dat, all_of(mass_dat))
            norm1 <- .norm_custom(y = intense1, custom_y = custom_int1)
            norm2 <- .norm_custom(y = intense2, custom_y = custom_int2)
            norm3 <- .norm_custom(y = intense3, custom_y = custom_int3)
            norm4 <- .norm_custom(y = intense4, custom_y = custom_int4)
            norm5 <- .norm_custom(y = intense5, custom_y = custom_int5)
            
            dat[spec1] <- norm1
            dat[spec2] <- norm2
            dat[spec3] <- norm3
            dat[spec4] <- norm4
            dat[spec5] <- norm5
            return(dat)}
        }}
      
      # Six spectra
      else{
        mass <- dat[[mass_dat]]
        intense1 <- dat[[spec1]]
        intense2 <- dat[[spec2]]
        intense3 <- dat[[spec3]]
        intense4 <- dat[[spec4]]
        intense5 <- dat[[spec5]]
        intense6 <- dat[[spec6]]
        
        i <- which(startsWith(as.character(mass), norm_mz))
        custom_int1 <- intense1[i]
        custom_int1 <- custom_int1[which(custom_int1 != 0)]
        custom_int2 <- intense2[i]
        custom_int2 <- custom_int2[which(custom_int2 != 0)]
        custom_int3 <- intense3[i]
        custom_int3 <- custom_int3[which(custom_int3 != 0)]
        custom_int4 <- intense4[i]
        custom_int4 <- custom_int4[which(custom_int4 != 0)]
        custom_int5 <- intense5[i]
        custom_int5 <- custom_int5[which(custom_int5 != 0)]
        custom_int6 <- intense6[i]
        custom_int6 <- custom_int6[which(custom_int6 != 0)]
        
        if(length(custom_int1) > 1 | 
           length(custom_int2) > 1 | 
           length(custom_int3) > 1 |
           length(custom_int4) > 1 |
           length(custom_int5) > 1 |
           length(custom_int6) > 1){
          stop('More than one peak per spectra for the given m/z value. 
                     Please be more precise.')
        } else if(length(custom_int1) < 1 | 
                  length(custom_int2) < 1 | 
                  length(custom_int3) < 1 |
                  length(custom_int4) < 1 |
                  length(custom_int5) < 1 |
                  length(custom_int6) < 1){  
          'The selected maximum intensity is 0 in some spectra.'
        } else{
          dat <- select(dat, all_of(mass_dat))
          norm1 <- .norm_custom(y = intense1, custom_y = custom_int1)
          norm2 <- .norm_custom(y = intense2, custom_y = custom_int2)
          norm3 <- .norm_custom(y = intense3, custom_y = custom_int3)
          norm4 <- .norm_custom(y = intense4, custom_y = custom_int4)
          norm5 <- .norm_custom(y = intense5, custom_y = custom_int5)
          norm6 <- .norm_custom(y = intense6, custom_y = custom_int6)
          
          dat[spec1] <- norm1
          dat[spec2] <- norm2
          dat[spec3] <- norm3
          dat[spec4] <- norm4
          dat[spec5] <- norm5
          dat[spec6] <- norm6
          return(dat)}
      }
    }  
  }
}    


# -----------------------------------------------------------------------