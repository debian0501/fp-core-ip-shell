package de.maibornwolff.tictactoeshell.transport;


import lombok.Value;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;


@Value
public class Turn {

    @Valid
    @NotNull(message = "player may not be null")
    private Player player;

    @Valid
    @NotNull(message = "field may not be null")
    private Field field;


}
