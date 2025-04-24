
# UPDATE ALL METADATA AND RE-SAVE DATASETS

#   normally done while updating the data but this could retroactively update metadata for all datasets 

############################################################ #

# update all metadata for datasets in EJAM/data/

x = datapack('EJAM')

for (i in 1:length(x$Item)) { 
  assign( 
    stop('to be continued')  ###   ?????
  )
}
rm(i)
############################################################ #

# update all metadata for datasets from pins board, and resave to pins board   

 x = download_dynamic(
    'all'
  )

 for (i in 1:length(x$Item))  {
  
  stop('to be continued')
  
 
  x = metadata_add(
    
  )
  EJAM:::datawrite_to_pins(
    'all'
  )
  
}
rm(i)
############################################################ #

# save local copies of those on pins board

datawrite_to_local(
  # varnames = 
  
  stop('to be continued')
  
)

############################################################ #

# save locally on sharepoint those on pins board
# 
# file.copy(
#   from = x,
#   to = x,
#   overwrite = TRUE
# )


