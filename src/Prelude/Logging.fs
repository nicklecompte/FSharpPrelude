namespace Prelude

/// Logger definitions.
[<AutoOpen>]
module Logging =

    open System.IO
    
    /// Standard LogLevel definition.
    [<Struct>]
    type LogLevel =
        | Verbose
        | Debug
        | Info
        | Warn
        | Error
        | Fatal
        | Off
    with
        static member FromInt(x:int) =
            match x with
            | 0 -> ValueSome Verbose
            | 1 -> ValueSome Debug
            | 2 -> ValueSome Info
            | 3 -> ValueSome Warn
            | 4 -> ValueSome Error
            | 5 -> ValueSome Fatal
            | 6 -> ValueSome Off
            | _ -> ValueNone

        member x.ToInt() =
            match x with
            | Verbose -> 0
            | Debug -> 1
            | Info -> 2
            | Warn -> 3
            | Error -> 4
            | Fatal -> 5
            | Off -> 6

        static member FromString(x:string) =
            match x.ToLower() with
            | "verbose" -> ValueSome Verbose
            | "debug" -> ValueSome Debug
            | "info" -> ValueSome Info
            | "warn" -> ValueSome Warn
            | "error" -> ValueSome Error
            | "fatal" -> ValueSome Fatal
            | "off" -> ValueSome Off
            | _ -> ValueNone
            

    /// Somewhat abstract listing of the areas of the application.
    /// This is for debugging purposes [and also code coverage concerns].
    type ApplicationArea =
        /// Related to getting and parsing user input from keyboard or mouse.
        | UserInput
        /// Specifically graphics-related.
        | GraphicalOutput
        /// Building program state from, e.g., a save file.
        | UserDataProcessing
        /// Input from the save file.
        | UserDataReading
        /// Output to the save file.
        | UserDataWriting
        /// Input from static resources (e.g. graphics, dialogue scripts)
        | ResourceDataReading
        /// The debug console.
        | DeveloperConsole
        | Other of string
        | Details of ApplicationArea*string

    type LogMessage = {
        level: LogLevel
        applicationArea: ApplicationArea
        message: string
    }

    /// Where to write the logs to.
    type LogSink =
        | Filepath of FileInfo
        | IOStream of Stream

    type LoggerConfig = {
        activeLoggingLevels: LogLevel list
        sink : LogSink
    }

    type LogErrorMessage =
        | CouldNotFindSinkFile
        | UnspecifiedException of exn

    type LogMessageAction = 
        LoggerConfig -> LogMessage -> Result<unit, LogErrorMessage>
