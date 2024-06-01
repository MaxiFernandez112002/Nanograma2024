
import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

let pengine;

function Game() {
    // State
    const [grid, setGrid] = useState(null);
    const [rowsClues, setRowsClues] = useState(null);
    const [colsClues, setColsClues] = useState(null);
    const [waiting, setWaiting] = useState(false);
    const [filasSatisfechas, setFilasSatisfechas] = useState([]);
    const [columnasSatisfechas, setColumnasSatisfechas] = useState([]);
    const [selectedContent, setSelectedContent] = useState('#'); // seteamos el contenido por default en '#'
    const [gameOver, setGameOver] = useState(false); // Nuevo estado para controlar el fin del juego
    const [statusText, setStatusText] = useState('MODO CLASICO'); // Estado para el texto de estado

    useEffect(() => {
        // Creation of the pengine server instance.
        // This is executed just once, after the first render.
        // The callback will run when the server is ready, and it stores the pengine instance in the pengine variable.
        PengineClient.init(handleServerReady);
    }, []);

    function handleServerReady(instance) {
        pengine = instance;
        const queryS = 'init(RowClues, ColumClues, Grid)';
        pengine.query(queryS, (success, response) => {
            if (success) {

                setGrid(response['Grid']);
                setRowsClues(response['RowClues']);
                setColsClues(response['ColumClues']);

                
                const rowsCluesS = JSON.stringify(response['RowClues']);
                const colsCluesS = JSON.stringify(response['ColumClues']);
                const squaresS = JSON.stringify(response['Grid']).replaceAll('"_"', '_');
                
                const queryG = `comprobar_grilla_react(${squaresS}, ${rowsCluesS}, ${colsCluesS}, FilaConPistasInvertida, ColumnaConPistasInvertida)`; 
                pengine.query(queryG, (success, responsep) => {
                    if (success) {
                        setFilasSatisfechas(responsep['FilaConPistasInvertida']);
                        setColumnasSatisfechas(responsep['ColumnaConPistasInvertida']);
                    }

                });
  
            }
        });
    }

    function handleClick(i, j, content) {
        // No action on click if we are waiting or if the game is over.
        if (waiting || gameOver) {
            return;
        }
        // Build Prolog query to make a move and get the new satisfaction status of the relevant clues.
        const squaresS = JSON.stringify(grid).replaceAll('"_"', '_'); // Remove quotes for variables.
        const rowsCluesS = JSON.stringify(rowsClues);
        const colsCluesS = JSON.stringify(colsClues);
        const queryS = `put("${content}", [${i},${j}], ${rowsCluesS}, ${colsCluesS}, ${squaresS}, ResGrid, RowSat, ColSat, NonogramaCompletado)`;

        // Set waiting state to true while waiting for response from server.
        setWaiting(true);
        // Send query to server using pengine instance.
        pengine.query(queryS, (success, response) => {
            if (success) {
                // Update the grid with the new content.
                setGrid(response['ResGrid']);

                if (response['RowSat'] === 1)
                    filasSatisfechas[i] = 1;
                else
                    filasSatisfechas[i] = 0;

                if (response['ColSat'] === 1)
                    columnasSatisfechas[j] = 1;
                else
                    columnasSatisfechas[j] = 0;
            }

            if (response['NonogramaCompletado'] === 1) {
                //alert("¡Felicidades! ¡Has ganado!");
                setGameOver(true); // El juego ha terminado
                setStatusText("¡Felicidades! ¡Has ganado!"); // Cambiar el texto de estado
            }

            // Set waiting state back to false after receiving response.
            setWaiting(false);
        });

    }

    function handleToggleContent() {
        setSelectedContent(selectedContent === 'X' ? '#' : 'X');
    }

    function handleAyuda() {
       /*COMPLETAR*/ 
    }

    function handleShowSolution() {
        /*COMPLETAR*/ 
    }


    if (!grid) {
        return null;
    }

    function handleRestart() {
        window.location.reload();
    }

    return (
        <div>
            <Board
                grid={grid}
                rowsClues={rowsClues}
                colsClues={colsClues}
                onClick={(i, j) => handleClick(i, j, selectedContent)}
                filasSatisfechas={filasSatisfechas}
                columnasSatisfechas={columnasSatisfechas}
            />
            <div>
                <button className='boton-modo' onClick={handleToggleContent}>Cambiar a modo {selectedContent === 'X' ? '#' : 'X'}</button>
                <button className='boton-ayuda' onClick={handleAyuda}>Revelar celda</button>
                <button className='boton-reiniciar' onClick={handleRestart}>Reiniciar Juego</button>
                <button className='boton-solucion' onClick={handleShowSolution}>Mostrar Solución</button>
            </div>
            <div className="game-info">
                {statusText}
            </div>
        </div>
    );
}

export default Game; 

