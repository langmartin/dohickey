const Tab = (function () {
    function newRow () {
        return document.createElement("tr").append(
            document.createElement("th").addEventListener("click", App.text)
        );
    }

    function add_tab(row, col) {
        const tb = document.querySelector("table tbody");
        for (var i=1; i<=row; i++) {

            const tr = tb.querySelector(`tr:nth-child(${row})`) || newRow();

            for (var j=1; j<=col; j++) {
                var el = tr.querySelector(`td:nth-child(${col})`);
                if (el) continue;

                const td = document.createElement("td");
                td.id = i + "-" + j;
                td.addEventListener("click", App.text);
                tr.append(td);
            }

            if (tr.childElementCount > 0) {
                tb.append(tr);
            }
        }
    }

    function add_row(row) {
        const tr = document.querySelector("table thead");
        const col = tr.childElementCount - 1;
        add_tab(row, col);
    }

    function add_col(col) {
        const tr = document.querySelector("table tbody");
        const row = tr.childElementCount - 1;
        add_tab(row, col);
    }

    return {
        add_row: add_row,
        add_col: add_col
    };
})();
