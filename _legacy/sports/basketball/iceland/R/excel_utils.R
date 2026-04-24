#' @export
repair_excel_file_mac <- function(input_path, output_path = input_path) {
  # Create a temporary AppleScript file
  script_path <- tempfile(fileext = ".scpt")

  # Write AppleScript commands to open, save, and close Excel file
  script_content <- sprintf(
    '
tell application "Microsoft Excel"
  open "%s"
  tell active workbook
    save
    close saving yes
  end tell
  if (count of workbooks) is 0 then
    quit
  end if
end tell
',
    normalizePath(input_path)
  )

  # Write script to file
  writeLines(script_content, script_path)

  # Execute the AppleScript
  system(sprintf("osascript %s", shQuote(script_path)))

  # Remove temporary script
  unlink(script_path)

  return(input_path)
}
