import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

let pengine;

function Game() {
    const [grid, setGrid] = useState(null);
    const [rowsClues, setRowsClues] = useState(null);
    const [colsClues, setColsClues] = useState(null);
    const [waiting, setWaiting] = useState(false);
    const [filasSatisfechas, setFilasSatisfechas] = useState([]);
    const [columnasSatisfechas, setColumnasSatisfechas] = useState([]);
    const [selectedContent, setSelectedContent] = useState('#');
    const [gameOver, setGameOver] = useState(false);
    const [statusText, setStatusText] = useState('MODO CLASICO');
    const [showingSolution, setShowingSolution] = useState(false);
    const [solutionGrid, setSolutionGrid] = useState(null);
    const [previousGrid, setPreviousGrid] = useState(null);
    const [revealingMode, setRevealingMode] = useState(false);
    const [revelarCeldaColor, setRevelarCeldaColor] = useState('#808080'); // Inicializa con el color original



    useEffect(() => {
        PengineClient.init(handleServerReady);
    }, []);

    function handleServerReady(instance) {
        pengine = instance;
        const queryS = 'init(RowClues, ColumClues, Grid)';
        pengine.query(queryS, (success, response) => {
            if (success) {
                const initialGrid = response['Grid'];
                setGrid(initialGrid);
                setRowsClues(response['RowClues']);
                setColsClues(response['ColumClues']);
                
                const rowsCluesS = JSON.stringify(response['RowClues']);
                const colsCluesS = JSON.stringify(response['ColumClues']);
                const squaresS = JSON.stringify(initialGrid).replaceAll('""', '_');
                const squaresX = JSON.stringify(initialGrid).replaceAll('"_"', '_');

                const querySolution = `solucion(${squaresX}, ${rowsCluesS}, ${colsCluesS}, GrillaSolucionada)`;
                pengine.query(querySolution, (success, responsepz) => {
                    if (success) {
                        console.log("Grilla Solucionada:", responsepz["GrillaSolucionada"]);
                        setSolutionGrid(responsepz["GrillaSolucionada"]);
                    } else {
                        console.error("Error al obtener la grilla solucionada:", responsepz);
                    }
                });

                const queryG = `comprobar_grilla_react(${squaresS}, ${rowsCluesS}, ${colsCluesS}, FilaConPistasInvertida, ColumnaConPistasInvertida)`; 
                pengine.query(queryG, (success, responsep) => {
                    if (success) {
                        setFilasSatisfechas(responsep['FilaConPistasInvertida']);
                        setColumnasSatisfechas(responsep['ColumnaConPistasInvertida']);
                    } else {
                        console.error("Error al comprobar la grilla:", responsep);
                    }
                });
            } else {
                console.error("Error al inicializar el juego:", response);
            }
        });
        console.log("Sali exitosamente de handleServerReady");
    }

    function handleClick(i, j, content) {
        if (waiting || gameOver) {
            return;
        }
    
        const squaresS = JSON.stringify(grid).replaceAll('"_"', '_');
        const rowsCluesS = JSON.stringify(rowsClues);
        const colsCluesS = JSON.stringify(colsClues);
    
        console.log("RevealingMode activado? : ", revealingMode);
        console.log("Celda clickeada : ", i, j);
        console.log("Contenido de la celda: ", grid[i][j]);
    
        if (revealingMode && grid[i][j] === '_') {
            content = solutionGrid[i][j];
            console.log("Entre a revealing mode y el contenido a colocar es: ", content);
        } else {
            console.log("No entre a revealing mode y el contenido a colocar es: ", content);
        }
    
        const queryS = `put("${content}", [${i},${j}], ${rowsCluesS}, ${colsCluesS}, ${squaresS}, ResGrid, RowSat, ColSat, NonogramaCompletado)`;
    
        setWaiting(true);
    
        pengine.query(queryS, (success, response) => {
            console.log("");
            if (success) {
                console.log("Entre al sucess");
                setGrid(response['ResGrid']);
    
                filasSatisfechas[i] = response['RowSat'] === 1 ? 1 : 0;
                columnasSatisfechas[j] = response['ColSat'] === 1 ? 1 : 0;
    
                if (response['NonogramaCompletado'] === 1) {
                    setGameOver(true);
                    setStatusText("¡Felicidades! ¡Has ganado!");
                }
            }
    
            setWaiting(false);
        });
    }
    

    function handleToggleContent() {
        setSelectedContent(selectedContent === 'X' ? '#' : 'X');
    }

   // Función para manejar el cambio de estado del modo de revelación
    function handleToggleRevealingMode() {
        setRevealingMode(!revealingMode);
        setRevelarCeldaColor(revelarCeldaColor === '#808080' ? '#90EE90' : '#808080');
        setRevealingMode(!revealingMode);
    }

    function handleShowSolution() {
        setShowingSolution(!showingSolution);
        if (!showingSolution && solutionGrid) {
            setPreviousGrid(grid); // Almacena la grilla actual antes de mostrar la solución
            setGrid(solutionGrid);
            setStatusText('MODO VER SOLUCION');
        } else {
            if (previousGrid) {
                setGrid(previousGrid); // Restaura la grilla desde el estado anterior
            }
            setStatusText('MODO CLASICO');
        }
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
                disableClicks={showingSolution} // Deshabilitar clics si se está mostrando la solución
            />
            <div>
                <button className='boton-modo' onClick={handleToggleContent}>Cambiar a modo {selectedContent === 'X' ? '#' : 'X'}</button>
                <button className='boton-revelar-celda' style={{ backgroundColor: revelarCeldaColor }} onClick={handleToggleRevealingMode}>Revelar celda</button>
                <button className='boton-reiniciar' onClick={handleRestart}>Reiniciar Juego</button>
                <button className='boton-solucion' onClick={handleShowSolution}>{showingSolution ? 'Ocultar Solución' : 'Mostrar Solución'}</button>
            </div>
            <div className="game-info">
                {statusText}
            </div>
        </div>
    );
}

export default Game;
