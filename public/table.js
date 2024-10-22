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

    function isEl(parent, el, idx, onClick, text) {
        console.log([el, idx])

        var x = parent.querySelector(`${el}:nth-child(${idx + 1})`);
        if (x) return x;
        x = document.createElement(el);
        if (onClick) x.addEventListener("change", onClick);
        x.textContent = text;
        x.contentEditable=true;
        parent.appendChild(x);
        return x;
    }

    function dim(row, col) {
        const r = Math.max(row, the_dim.row);
        const c = Math.max(col, the_dim.col);
        the_dim.row = r;
        the_dim.col = c;

        console.log([r, c])

        const tb = document.querySelector(`table tbody`);

        for (var i=1; i<=r; i++) {
            var tr = isEl(tb, "tr", i);
            isEl(tr, "th", 0, App.onEdit(i, 0), "Goal");

            for (var j=1; j<=c; j++) {
                isEl(tr, "td", j, App.onEdit(i, j));
            }
        }

        for (j=1; j<=c; j++) {
            const tr0 = document.querySelector(`table thead tr`);
            isEl(tr0, "th", j, App.onEdit(0, j), "Option");
        }
    }

    return {
        dim: dim,
        init: init
    };
})();
