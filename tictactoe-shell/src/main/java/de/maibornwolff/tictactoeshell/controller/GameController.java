package de.maibornwolff.tictactoeshell.controller;

import de.maibornwolff.tictactoeshell.transport.*;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

import static de.maibornwolff.tictactoecore.business.Game.newGame;
import static de.maibornwolff.tictactoecore.business.Game.game;

@RestController
public class GameController {

    private GameState gameState = newGame();


    @GetMapping("/newGame")
    public GameState startGame() {
        gameState = newGame();
        return gameState;

    }

    @PostMapping("/move")
    public GameState startGame(@Valid @RequestBody Field field ) {
        Move move = new Move(field, gameState);
        gameState = game(move);
        return gameState;
    }

}