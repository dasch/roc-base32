app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br" }

import Base32
import Utils
import pf.Stdout
import pf.Path
import pf.File
import pf.Utc
import pf.Task exposing [Task]

devRandom = Path.fromStr "/dev/urandom"

# A task that generates 10 bytes (80 bits) of random data.
randomInt =
    File.readBytes devRandom
    |> Task.map \bytes -> List.takeFirst bytes 10
    |> Task.mapErr \_ -> RandomnessGenerationFailed

encode = \ts, rand ->
    # 6 bytes / 48 bits from the timestamp (least significant bits)
    tsBytes =
        Utc.toMillisSinceEpoch ts
        |> Utils.intToBytes
        |> List.takeLast 6

    bytes = List.concat tsBytes rand

    expect List.len bytes == 16

    Base32.encodeBytes bytes

generate =
    Task.await Utc.now \ts ->
        Task.map randomInt \rand ->
            encode ts rand

main =
    ulid = generate!
    Stdout.line! "ULID: $(ulid)"
