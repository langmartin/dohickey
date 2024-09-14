const Table = (function () {
    /*
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
    */

    const the_dim = {row: 0, col: 0};

    function add_option() {
        dim(the_dim.row, the_dim.col + 1);
    }

    function add_goal() {
        dim(the_dim.row + 1, the_dim.col);
    }

    function init() {
        document.querySelector("#add_option").addEventListener("click", add_option);
        document.querySelector("#add_goal").addEventListener("click", add_goal);
    }

    function istr(row) {
        const tb = document.querySelector(`table tbody`);
        var tr = tb.querySelector(`tr:nth-child(${row + 1})`);
        if (tr) return;

        tr = document.createElement("tr");
        const th = document.createElement("th");
        th.addEventListener("click", App.text);
        th.textContent = "Goal";
        tr.append(th);
        tb.appendChild(tr);
    }

    function isth(col) {
        const tr = document.querySelector(`table thead tr`);
        var th = tr.querySelector(`td:nth-child(${col + 1})`);
        if (th) return;
        th = document.createElement("th");
        th.textContent = "Option";
        th.id = "0-" + col;
        th.addEventListener("click", App.text);
    }

    function istd(row, col) {
        const tb = document.querySelector(`table tbody`);
        const tr = tb.querySelector(`tr:nth-child(${row + 1})`);
        var td = tr.querySelector(`td:nth-child(${col + 1})`);
        if (td) return;

        td = document.createElement("td");
        td.id = row + "-" + col;
        td.addEventListener("click", App.text);

        console.log(td)

        tr.append(td);
    }

    function dim(row, col) {
        const r = Math.max(row, the_dim.row);
        const c = Math.max(col, the_dim.col);

        the_dim.row = r;
        the_dim.col = c;

        for (var i=0; i<r; i++) {
            istr(i);
            for (var j=0; j<c; j++) {
                istd(i, j);
            }
        }
    }

    return {
        dim: dim,
        init: init
    };
})();
