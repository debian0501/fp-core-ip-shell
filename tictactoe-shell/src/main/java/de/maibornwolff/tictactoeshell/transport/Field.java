package de.maibornwolff.tictactoeshell.transport;

import lombok.Value;

import javax.validation.constraints.NotNull;

@Value
public class Field {

    @NotNull(message = "x may not be null")
    private int x;

    @NotNull(message = "y may not be null")
    private int y;

}
