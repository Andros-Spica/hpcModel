animateTrajectory <- function(file_path, file_name, delete_frames = TRUE)
{
  require(purrr)
  require(magick)
  
  # generate GIFs with each batch and delete used images
  
  files <- paste0(file_path, dir(path = file_path, pattern = paste0("^", file_name, ".*\\.png$")))
  
  cat('There are ', length(files), '\n')
  
  images <- map(files, image_read)
  images <- image_join(images)
  
  cat('Preparing animation ...')
  
  animation <- image_animate(images, fps = 10, dispose = "previous", optimize = TRUE)
  
  cat('done.\n')
  
  cat('Generating GIF ...')
  
  image_write(animation, paste0(file_path, file_name, "-anim.gif"))
  
  cat('done.\n')
  
  if (delete_frames) { unlink(files); }
  
}
# 
# animateTrajectory_animateBatches <- function(file_path, file_name, delete_frames = TRUE, batch_length = 50)
# {
#   require(purrr)
#   require(magick)
#   
#   # generate GIFs with each batch and delete used images
#   
#   files <- paste0(file_path, dir(path = file_path, pattern = paste0("^", file_name, ".*\\.png$")))
#   
#   # split the total number of files into batches
#   # This is to improve performance, due to memory usage issues.
#   indexes <- seq_along(files)
#   batches <- split(files, ceiling(indexes/batch_length))
#   
#   cat('There are ', length(files), ' images, split into ', length(batches), 'batches: ', paste(lapply(batches, length)), '\n')
#   cat('Generating GIFs per batch of images...\n')
#   
#   # iterate for each batch and generate GIFs
#   batchIndex = 0
#   for (aBatch in batches)
#   {
#     images <- map(aBatch, image_read)
#     images <- image_join(images)
#     
#     animation <- image_animate(images, fps = 10, dispose = "previous", optimize = TRUE)
#     
#     image_write(animation, paste0(file_path, file_name, "-anim_", batchIndex, ".gif"))
#     
#     if (delete_frames) { unlink(aBatch); }
#     
#     cat('Batch ', batchIndex, ' done.\n')
#     
#     batchIndex = batchIndex + 1
#   }
#   
#   cat('All partial GIFs are done.\n')
# }
# 
# animateTrajectory_joinAnimations <- function(file_path, file_name, delete_batch_gifs = TRUE)
# {
#   require(purrr)
#   require(magick)
#   
#   # combine the GIF animations into a single GIF animation and delete used gifs
#   
#   files <- paste0(file_path, dir(path = file_path, pattern = paste0("^", file_name, "-anim_", ".*\\.gif$")))
#   
#   animations <- map(files, image_read)
#   animations <- image_join(animations)
#   
#   finalAnimation <- image_convert(animations)
#   
#   image_write(finalAnimation, paste0(file_path, file_name, "-anim.gif"))
#   
#   if (delete_batch_gifs) { unlink(files); }
#   
#   cat('Final GIF is done.\n')
# }
