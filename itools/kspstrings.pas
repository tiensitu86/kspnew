unit KSPStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  SScanning = 'Scanning folders...';

  SFile='File:';
SPlayingNewFile='New File - KSP';
SLastSongPlayed='Last Song - KSP';
SLastSongPlayed2='Last song played';
SElapsed='Elapsed:';
SRemaining='Remaining:';
SNewestRelease='The newest release is:';
SDownloadNow='Download Now';
SRepeatOne='Repeat ONE';
SRepeatAll='Repeat ALL';
SRepeatOff='OFF';
STagArtist='Artist:';
STagAlbum='Album:';
STagYear='Year:';
STagComment='Comment:';
STagGenreID='Genre-ID:';
STagGenre='Genre:';
STitle = 'Title:';
SLength = 'Length:';
//SFavourite = 'Favourite:';
SUnknownAlbum = 'Unknown Album';
SUnknownArtist = 'Unknown Artist';
SUnknownTitle = 'Unknown Title';
SUnknownYear = 'Unknown Year';
SUnknownGenre = 'Unknown Genre';
SPlaylists = 'Playlists';
SNativeFiles = 'Native file formats';
//SInvalidPlugin = 'Invalid plug-in.';
//SReallyScan = 'This process may tak a few minutes an during it KSP can not be able to respond.\nAfter finish the message dialog will appear and you will be able to use KSP again without restart\nReally continue?';
SScanningDone = 'Media library has been rescaned sucessfully. Now you can continue your work';
//SFileError = 'An error occurred while reading from the file';
SPlaylist = 'Playlist';
//SSuggOptions = 'Suggestion Options';
//SSuggestions = 'Info';
//SConnecting = 'Connecting to host...';
//SSuccess = 'Success!';
//SLogging = 'Logging into';
//SSendingQuery = 'Sending query for album';
//SReadingInfo = 'Reading info...';
//SFinishing = 'Finishing...';
//SCancel = 'Cancelling...';
//SWizardCanceled = 'Wizard aborted. Do you want to re-run it next time?';
SMigratedToNew = 'Media Items database migrated into new version.\nOld entries are still available.\nIf you do not experience any problems with the new database\nyou can safely remove them';
SDuplicatedItems = 'Because of some reason in Media Library there are\nsome items duplicated. KSP will now search for this duplicated items\nand eliminate them. This process can tak a few minutes and computer can\nslow down for this time. Please, be patient';
//SOutputType = 'Using %s output';
SNo = 'No';
SYes = 'Yes';
SToLeftOutput = '%s%% to left speaker';
SToRightOutput = '%s%% to right speaker';
SToCenterOutput = 'Set to center';
//SRegisteredNewAccount = 'New account is registered, Your number is: %s\nYou may now login to your new account';
SOnMinimizeToTrayAlert = 'You have minimized KSP to system tray, To restore it just right click on KSP icon. You can disable this alert in KSP options';
SClose = 'Close';
//SCloseThisAlert = 'This alert is only informational so there is no need to display it.\nDo you want to disable it?\nDisabled alert can be restored in KSP options';
SMetaContent = 'ICECAST: Playing %s';
SShoutcastAddressCaption = 'Add URL:';
SShoutcastAddressDesc = 'Enter the URL to be played';
SShoutcastEntry = 'ICECAST: %s';
SAutoSupportName = 'Give your name:';
SAutoSupportNameText = 'Give your name:';
SAutoSupportEMail = 'Give your e-mail address:';
SAutoSupportEMailText = 'Give your e-mail address';
SCustomHintPlaylist = 'KSP Playlist Editor Quick Hint';
sXMLIncorrectIndex = 'XML Parser: Incorrect Index: %s';
sXMLIncorrectCharLower = 'XML Parser: Char < is expected';
sXMLIncorrectEOF = 'XML Parser: Unexpected end of file';
sXMLUnexpectedCloseTag = 'XML Parser: Unexpected closing tag';
sXMLIncorrectTagName = 'XML Parser: Incorrect tag name';
sXMLNoEOC = 'XML Parser: Unexpected end of file. Comment is not closed';
sXMLIncorrectEOFParams = 'XML Parser: Unexpected end of file. Tag closing or a parameter was expected';
sXMLIncorrectCharParamName = 'XML Parser: Incorrect char in parameter name';
sXMLIncorrectEOFEqual = 'XML Parser: Unexpected end of file. = was expected';
sXMLIncorrectCharEqual = 'XML Parser: Incorrect char. = was expected';
sXMLIncorrectEOFB = 'XML Parser: Unexpected end of file. " was expected';
sXMLIncorrectCharB = 'XML Parser: Incorrect end of file. " was expected';
sXMLIncorrectEOFNoParam = 'XML Parser: Unexpected end of file. A parameter was not closed';
sXMLIncorrectChar = 'XML Parser: Incorrect char';
sXMLIncorrectNoClosingTag = 'XML Parser: Unexpected end of file. A closing tag was expected';
sXMLError = 'XML Parser: An error occured while parsing, line: %s, char: %s, message: %s';
//SAdminPrivReqOldSys = 'To Associate files with KSP you need an Administrator priviliges. Run this Wizard again as an Administrator to associate file extensions with KSP';
//SOverwrite = 'File already exists. Overwrite it?';
SCannotDeleteRenameDefaulEq = 'Any default setting of Equalizer cannot be renamed or deleted.\nYou can only rename or delete settings created by you';
SEQNameInputCaption = 'Preset Name:';
SEQNameInput = 'Preset name:';
//SAutoSupportText2 = 'You have chosen not to use Automatic Support feature. To enable this feature next time go to Options->Other->Automatic Support';
sRestartNeeded = 'This feature will be changed/enabled after restart';
//SRenameFileFolderCaption = 'Rename item...';
//SRenameFileFolder = 'Rename selected item to:';
SInputBookmarkCaption = 'Bookmark name:';
SInputBookmarkPrompt = 'Bookmark name:';
SBookmarkDelete = 'Are you sure you want to remove bookmark %s?';
SBookmarkDeleteCaption = 'Delete bookmark';
SSampleAlert = 'Sample alert text';
SSampleAlertCaption = 'Sample alert caption';
SDBMemo = 'KSP requires connection to database.\nCurrently MySQL and Sqlite3 database is supported.\nProvide valid database information in order to\n'+
  'configure application.\n\nIMPORTANT:\n\nKSP requires MySQL username that has FULL rights\nto chosen database (create table, insert, delete, alter,\n'+
  'select). Additionally if that user has create database\nright it is possible thatKSP will create database if chosen\n'+
  'database is missing. Otherwise you have to create it manually';
SPluginsLoaded = 'All enabled plugins are loaded';
SPluginsUnloaded = 'All enabled plugins are unloaded';
SPluginsDisabled = 'All currently installed plugins are disabled';
SPluginsEnabled = 'All currently installed plugins are enabled';

implementation

end.

