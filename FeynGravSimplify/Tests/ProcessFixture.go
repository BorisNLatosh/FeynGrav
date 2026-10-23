// Test double only. Build explicitly with go build; it is not a runtime dependency.
package main

import (
    "fmt"
    "io"
    "os"
    "strconv"
    "strings"
    "time"
)

func main() {
    mode := os.Getenv("FG_CONTRACTOR_TEST_MODE")
    if path := os.Getenv("FG_CONTRACTOR_TEST_PID"); path != "" {
        _ = os.WriteFile(path, []byte(strconv.Itoa(os.Getpid())), 0600)
    }
    if mode == "early" { os.Exit(17) }
    input, _ := io.ReadAll(os.Stdin)
    if len(os.Args) > 1 && os.Args[len(os.Args)-1] == "input.frm" {
        result := "FGFORMBEGIN\n0\nFGFORMEND\n"
        switch mode {
        case "exit": fmt.Fprintln(os.Stderr, "FORM fixture failure"); os.Exit(7)
        case "empty": result = ""
        case "truncated": result = "FGFORMBEGIN\n0"
        case "unknown": result = "FGFORMBEGIN\nunknown\nFGFORMEND\n"
        case "stderr": fmt.Fprint(os.Stderr, strings.Repeat("diagnostic\n", 200000))
        case "wait": time.Sleep(20 * time.Second)
        case "progress": time.Sleep(300 * time.Millisecond)
        }
        _ = os.WriteFile("result.txt", []byte(result), 0600)
        return
    }
    switch mode {
    case "exit": fmt.Fprintln(os.Stderr, "test failure"); os.Exit(7)
    case "empty": return
    case "truncated": fmt.Print("[\"Plus\",")
    case "unknown": fmt.Print("\"not-in-the-mapping\"")
    case "unsafe": fmt.Print("[\"Run\",\"not-a-command\"]")
    case "stderr": fmt.Fprint(os.Stderr, strings.Repeat("diagnostic\n", 200000)); fmt.Print("0")
    case "progress": time.Sleep(11 * time.Second); fmt.Print("0")
    case "wait": time.Sleep(20 * time.Second); fmt.Print("0")
    case "threads":
        n := 0
        if len(os.Args) == 3 && os.Args[1] == "-threads" { n, _ = strconv.Atoi(os.Args[2]) }
        fmt.Print(n)
    default: os.Stdout.Write(input)
    }
}
