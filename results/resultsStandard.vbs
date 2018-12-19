Option Explicit

Dim xlApp, xlBook

Set xlApp = CreateObject("Excel.Application")

Set xlBook = xlApp.Workbooks.Open(CreateObject("Scripting.FileSystemObject").GetParentFolderName(WScript.ScriptFullName) & "\Standard.xlsm", 0, True)
xlApp.Run "ImportCSV"
xlBook.Close False

Set xlBook = xlApp.Workbooks.Open(CreateObject("Scripting.FileSystemObject").GetParentFolderName(WScript.ScriptFullName) & "\StandardAll.xlsm", 0, True)
xlApp.Run "ImportCSV"
xlBook.Close False

xlApp.Quit

Set xlBook = Nothing
Set xlApp = Nothing

WScript.Quit
