export function maximizeBox() {
  
}


export function create_legend(titles, colors) {
	  let legend_div = document.getElementById('legend');
	
    let legend_item = document.createElement('div');
    let col_circle = document.createElement('div');
    let legend_span = document.createElement('span');

    legend_item.classList.add('legend-item');
    col_circle.classList.add('legend-col-circle');
    legend_span.classList.add('legend-span');

    col_circle.style.backgroundColor = color;
    legend_span.textContent = title;

    legend_item.append(col_circle);
    legend_item.append(legend_span);

    legend_div.append(legend_item);

}