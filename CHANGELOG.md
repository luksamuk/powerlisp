# Changelog

## 1.5.0 - 2018-09-03
### Added
- Add better support for `rofi` and other `rofi`-friendly features.
- Add customization option for displaying the launcher on bottom or screen
  or not (on by default).
- Add macro `with-powerlisp-user-input`, which only executes its body
  when the user entered something on the input field.
- Add macro `with-powerlisp-menu`, which is similar to
  `with-powerlisp-options-menu`, but does not require the input to be on the
  given alist, allowing to have access to the user input with the `raw-input`
  variable on the body, unless the user has not typed anything.
- Add private navigation support when calling the browser.

### Changed
- Refactor code completely.
- Reorganize sections of the code for clarity.
- Changed names of variables for launcher-related customizations.
- Fix `dmenu` customization options which would not work.

### Removed
- Zeal support. This should be a user-defined feature.
  Examples are maintained on the Wiki.
- Default favorite websites and commands.
  Those should be a personal choice of the user.
  

## 1.0.0 - 2018-06-05
### Added
- Port bash version's basic, expected features.
- Add Zeal support.
- Add basic customization API.
- Add trivial aesthetic settings.
- Add FASL compilation instructions to README.
  
