(* Test-only completion record. Direct standalone suite invocation remains
   usable; a record is written only when the driver supplies CFC_TEST_DIR. *)
writeSuiteCompletion[suite_String, passed_Integer, failed_Integer] := Module[
    {directory, path},
    directory = Environment["CFC_TEST_DIR"];
    If[StringQ[directory] && DirectoryQ[directory],
        path = FileNameJoin[{directory, "suite-completed-" <> suite <> ".json"}];
        Export[path,
            <|"Suite" -> suite, "Passed" -> passed, "Failed" -> failed,
              "Status" -> If[failed === 0, "Completed", "Failed"]|>,
            "RawJSON"];
    ];
];
