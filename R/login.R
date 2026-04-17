#' Lengjan Login
#'
#' Authenticate to games.lotto.is and return a chromote session.
#' Credentials are read from environment variables LENGJAN_USER and LENGJAN_PASS.

box::use(
  chromote[ChromoteSession]
)

#' Create an authenticated Lengjan browser session
#'
#' @param headless Run headless? (default TRUE for CI, FALSE for debugging)
#' @return A ChromoteSession object, logged in to Lengjan
lengjan_login <- function(headless = TRUE) {
  user <- Sys.getenv("LENGJAN_USER")
  pass <- Sys.getenv("LENGJAN_PASS")

  if (user == "" || pass == "") {
    stop(
      "LENGJAN_USER and LENGJAN_PASS must be set in .Renviron\n",
      "Add these lines to your .Renviron:\n",
      "  LENGJAN_USER=your_username\n",
      "  LENGJAN_PASS=your_password"
    )
  }

  cli::cli_alert_info("Opening Lengjan...")
  session <- ChromoteSession$new()
  if (!headless) session$view()

  # Patch navigator.webdriver before any page loads - this is the primary
  # signal checked by anti-bot systems when Chrome is driven via CDP.
  session$Page$addScriptToEvaluateOnNewDocument(
    source = "Object.defineProperty(navigator, 'webdriver', {get: () => undefined})"
  )

  session$Page$navigate("https://games.lotto.is")
  Sys.sleep(4)

  # Click "Minar sidur" (M\u00ednar s\u00ed\u00f0ur) to open login modal
  cli::cli_alert_info("Opening login modal...")
  js_open_login <- "
    (() => {
      const buttons = document.querySelectorAll('button');
      for (const btn of buttons) {
        if (btn.textContent.trim() === 'M\u00ednar s\u00ed\u00f0ur') {
          btn.click();
          return true;
        }
      }
      return false;
    })()
  "
  result <- session$Runtime$evaluate(expression = js_open_login, returnByValue = TRUE)
  if (!isTRUE(result$result$value)) {
    stop("Could not find 'M\u00ednar s\u00ed\u00f0ur' button.")
  }
  Sys.sleep(2)

  # Fill in credentials using the form inputs.
  # jsonlite::toJSON produces a safely-escaped JSON string, avoiding breakage
  # if the password contains single quotes, backslashes, or other JS-special chars.
  cli::cli_alert_info("Entering credentials...")
  user_js <- jsonlite::toJSON(user, auto_unbox = TRUE)
  pass_js <- jsonlite::toJSON(pass, auto_unbox = TRUE)
  js_fill <- sprintf("
    (() => {
      const userInput = document.getElementById('username');
      const passInput = document.getElementById('password');
      if (!userInput || !passInput) return false;

      const setNativeValue = (el, value) => {
        const setter = Object.getOwnPropertyDescriptor(
          window.HTMLInputElement.prototype, 'value'
        ).set;
        setter.call(el, value);
        el.dispatchEvent(new Event('input', { bubbles: true }));
        el.dispatchEvent(new Event('change', { bubbles: true }));
      };

      setNativeValue(userInput, %s);
      setNativeValue(passInput, %s);
      return true;
    })()
  ", user_js, pass_js)

  fill_result <- session$Runtime$evaluate(expression = js_fill, returnByValue = TRUE)
  if (!isTRUE(fill_result$result$value)) {
    stop("Could not find username/password inputs in login modal.")
  }
  Sys.sleep(0.5)

  # Click "Innskra" (Innskr\u00e1) submit button
  js_submit <- "
    (() => {
      const buttons = document.querySelectorAll('button');
      for (const btn of buttons) {
        if (btn.textContent.trim() === 'Innskr\u00e1' && btn.type === 'submit') {
          btn.click();
          return true;
        }
      }
      return false;
    })()
  "
  session$Runtime$evaluate(expression = js_submit, returnByValue = TRUE)
  Sys.sleep(4)

  # Verify login succeeded
  page_text <- session$Runtime$evaluate(
    expression = "document.body.textContent",
    returnByValue = TRUE
  )$result$value

  if (!grepl("kr\\.", page_text, ignore.case = TRUE)) {
    cli::cli_alert_warning("Login may have failed - no balance detected in page text.")
  } else {
    cli::cli_alert_success("Logged in to Lengjan successfully.")
  }

  session
}

#' Check if a session is still authenticated
#'
#' @param session A ChromoteSession
#' @return Logical
is_authenticated <- function(session) {
  page_text <- tryCatch(
    session$Runtime$evaluate(
      expression = "document.body.textContent",
      returnByValue = TRUE
    )$result$value,
    error = function(e) ""
  )
  grepl("kr\\.", page_text, ignore.case = TRUE)
}
