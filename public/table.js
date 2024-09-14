const Table = (function () {
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
        th.addEventListener("click", App.on_edit(row, 0));
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
        th.addEventListener("click", App.on_edit(0, col));
    }

    function istd(row, col) {
        const tb = document.querySelector(`table tbody`);
        const tr = tb.querySelector(`tr:nth-child(${row})`);
        var td = tr.querySelector(`td:nth-child(${col})`);
        if (td) return;

        td = document.createElement("td");
        td.id = row + "-" + col;
        td.addEventListener("click", App.on_edit(row, col));

        console.log(td)

        tr.append(td);
    }

    function dim(row, col) {
        const r = Math.max(row, the_dim.row);
        const c = Math.max(col, the_dim.col);

        the_dim.row = r;
        the_dim.col = c;

        for (var i=1; i<=r; i++) {
            istr(i);
            for (var j=1; j<=c; j++) {
                isth(j);
                istd(i, j);
            }
        }
    }

    return {
        dim: dim,
        init: init
    };
})();
