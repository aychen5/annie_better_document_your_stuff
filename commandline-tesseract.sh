# A Simple Shell Script To Get Linux Network Information
# run tesseract streaming from JPG to pdf
ls | grep \.jpg$ | sort -g| tesseract -c stream_filelist=true - - pdf > searchable.pdf
 

pdftoppm -r 300 ep-2013-03-04-kenya.pdf page

ls | grep \.ppm$ | sort -g| tesseract --oem 1 -c stream_filelist=true - - pdf > ep-2013-03-04-kenya-ocr.pdf


rm page-??????.ppm*