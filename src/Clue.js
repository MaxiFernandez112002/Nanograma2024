import React from 'react';

function Clue({ clue, satisfecha}) {
    return (
        <div className={satisfecha} >
            {clue.map((num, i) =>
                <div key={i}>
                    {num}
                </div>
            )}
        </div>
    );
}



export default Clue;