animate.trajectory <- function(filePath, fileName, delete.frames = TRUE)
{
  files <- paste0(filePath, dir(path = filePath, pattern = paste0("^", fileName, ".*\\.png$")))
  images <- purrr::map(files, image_read)
  images <- magick::image_join(images)
  animation <- magick::image_animate(images, fps = 10)
  magick::image_write(animation, paste0(filePath, fileName, "-anim.gif"))
  if (delete.frames) { unlink(files); }
}
