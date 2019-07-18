package de.maibornwolff.tictactoeshell.transport;

import lombok.Value;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;


@Value
public class GameState {

    @NotNull(message = "board may not be null")
    private int[][] board;

    @Valid
    @NotNull(message = "status may not be null")
    private Status status;

    @Valid
    @NotNull(message = "nextPlayer may not be null")
    private Player nextPlayer;

}
