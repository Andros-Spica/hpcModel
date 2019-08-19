animate.trajectory <- function(filePath, fileName, delete.frames = TRUE)
{
  require(purrr)
  require(magick)
  files <- paste0(filePath, dir(path = filePath, pattern = paste0("^", fileName, ".*\\.png$")))
  images <- map(files, image_read)
  images <- image_join(images)
  animation <- image_animate(images, fps = 10)
  image_write(animation, paste0(filePath, fileName, "-anim.gif"))
  if (delete.frames) { unlink(files); }
}
