namespace Prelude

/// A somewhat OO solution to dependencies.
/// These are highly stateful, effectful computations and datatypes,
/// yet they must be available in arbitrary locations throughout the game.
[<RequireQualifiedAccess>]
module DependencyManagement =
    open System.IO
(*
Some remarks are in order here:
1) While I like the standard OO service method of having an IService with ILogger properties that
each of the business logic services implement, I don't like the idea of having lots of IInitializeAfterCreates,
or generally having classes related to (say) CreatureAI having to manage things like dependency injection,
making sure the ILogger is initialized, and so on.

2) Specifically, I don't want classes at all! With a small number of exceptions, all of my classes
are datatypes and are updated by static functions.
3) My solution is this module. It essentially defines a few global variables and types
that can be accessed from anywhere in the RoguelikePrototype namespace.
4) In this sense, Dependencies are called similar to things like printf / failwith / etc -
their functionality exists "outside" of the Creature / Dungeon modules. The caller is reminded of this by
RequireQualifiedAccess.
*)

    /// Simple record type of dependencies.
    type Dependencies = {
        loggingAction : LogMessageAction
    }

    let mutable private defaultLogConfig = {
        activeLoggingLevels = [Debug; Info; Warn; Error; Fatal]
        sink = Filepath(FileInfo("logs/log.txt"))
    }

    let updateLoggingLevel (levels: LogLevel list) : Result<unit,LogErrorMessage> =
        //try
            let newConfig = {defaultLogConfig with activeLoggingLevels = levels}
            defaultLogConfig <- newConfig
            Ok(())
        //with
        //    :? System.Exception as ex -> Result.Error(UnspecifiedException ex)

    let updateLogSink (ls: LogSink) : Result<unit,LogErrorMessage> =
        match ls with
        | IOStream s -> failwith "not done"
        | Filepath p ->
            // Need to check that the filepath is valid.
            failwith "not done"
        Ok()

    //let defaultDependencies : Dependencies = {
    //    loggingAction = failwith "not done"
    //}
