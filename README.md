![FMView](FMView.png)
## FMView  ##

FMView is a Windows application designed to simplify review and interaction with
various components of Veterans Health Information Systems and Technology Architecture (VistA).
With FMView, users can:

- View the definition and contents of FileMan files, such as data dictionaries,
  file attributes, and data entries.
- Veiw/Edit M routines.
- View VistA globals.
- Review implementation and execute remote procedures.

FMView requires the XWB and FMDC packages to be installed on the server,
as well as a custom RPC (remote procedure call)
to access the files definitions and source of routines on VistA accounts.
This application is designed to work VistA instances that use hash tables "VistA", "VistA Demo" and "OSHERA".

### Disclaimer ###
The application enables the user to view and modify the data
on both server and client machines. However, it is not intended
to be a VistA data editing tool and it may cause data corruption or loss if used improperly.
To prevent any damage to your important data, please test the application
on a non-production DB instance first and make sure it is compatible
with your server software version before accessing any critical data.

> **Note:** The application is provided **"AS IS" WITH NO WARRANTIES**—the author is not responsible for any data loss or damage that may occur as a result of using this application.

### Privacy Policy ###
The application does not collect any personal information from the user.
It may create a log file that records the connection to VistA accounts
and RPC execution. This log file is stored on the local machine where the application is running.
Configuration information is saved in user's AppData\FMView folder.

### Implementation ###
FMView is built with Embarcadero Delphi 12 (Community Edition).
VistA Files data is accessed by FileMan Delphi Components (FMDC v1.0) library.
The information about definition of VistA components (files definition,
routines source, and globals contents) is provided by custom  RPC.
Communication with VistA servers is done by customized version of VistA Broker.
Packages SynEdit, VirtualTree (TurboPack) are used for
routine syntax highlighting and data presentation.

This repository contains second version of FMView.
Main differences with version 1:
- For communication with VistA accounts FMView v2 uses LeanXWB (XWB RPC Broker without dependence on Delphi VCL or CCOW support)
- User session data (opened windows, preferences) is saved/restored by default
- No support for MDI - different account viewers are kept on separate tabs within the main application window

### Support ###
Questions, suggestions, and criticisms are welcome at zzFMTools@gmail.com

### Credits ###
Special thanks to the [**WorldVistA open-source community**](https://worldvista.org/) for providing access to test databases, engaging in discussions, and offering invaluable feedback.

**2025.05.14**
