package de.maibornwolff.tictactoeshell.transport;


import lombok.Value;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;


@Value
public class Move {

    @Valid
    @NotNull(message = "turn may not be null")
    private Field field;

    @Valid
    @NotNull(message = "gameState may not be null")
    private GameState gameState;

}
