import React from 'react';

function Square({ value, onClick, disableClicks }) {
    const handleClick = () => {
        if (!disableClicks) {
            onClick();
        }
    };

    return (
        <button className="square" onClick={handleClick}>
            {value !== '_' ? value : null}
        </button>
    );
}

export default Square;
