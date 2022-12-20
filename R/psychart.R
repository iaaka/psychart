getPolar = function(a,xr,yr=xr,center=c(0,0)){
	list(x=center[1]+cos(a)*xr,y=center[2]+sin(a)*yr)
}

#' Plot psychart
#'
#' Creates new plot if center
#'
#' @param x a vector of non-negative numerical quantities. The values in x are displayed as the areas of pie slices.
#' @param labels character gibing names of slices
#' @param radius psypie radius in user coordinates (by x-axis, see norm.radius)
#' @param spiral.shift twisting size in degrees
#' @param init.angle number specifying the starting angle (in degrees) for the slices. Defaults to 0 (i.e., ‘3 o'clock’) unless clockwise is true where init.angle defaults to 90 (degrees), (i.e., ‘12 o'clock’).
#' @param col a vector of colors to be used in filling or shading the slices
#' @param center center of psypie.
#' @param tck.length relative tick lenght (as fraction from radius)
#' @param norm.radius logical, should psypie appear as cycle in device coordinates (defoult). Psypie will be cycle in user coordinates if norm.radius is FALSE
#' @param border,lty,lwd paramteres to be passed to polygon
#' @param axes logical whether axis shoul be plotted
#' @param new logical, should new plot be created (adds psypie to existing if FALSE)
#' @param N number of points used to plot each line segment. Set higher for better detalization.
#' @param xlab,ylab axis labels
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
psychart = function(x,labels = names(x),radius=0.4,spiral.shift=0,init.angle=0,col=rainbow(length(x)),border=NA,lty=1,lwd=1,
                    center=c(0,0),tck.length = radius/10,norm.radius=TRUE,axes=FALSE,xlab='',ylab='',new=TRUE,N=50,...){
	if(new)
		plot(0,type='n',axes=axes,xlim=center[1]+c(-radius,radius)*1.2,ylim=center[2]+c(-radius,radius)*1.2,xlab=xlab,ylab=ylab,...)
	x = x/sum(x)*2*pi
	angle = init.angle/180*pi
	spiral.shift = spiral.shift/180*pi

	yradiusscale = 1
	if(norm.radius){
	  yradiusscale=grconvertY(grconvertX(1:2,'user','inch'),'inch','user')
	  yradiusscale = yradiusscale[2] - yradiusscale[1]
	}

	for(i in 1:length(x)){
		a = c(seq(from=angle,to=angle+spiral.shift,length.out = N),
					seq(from=angle+spiral.shift,to=angle+spiral.shift+x[i],length.out = N),
					seq(from=angle+spiral.shift+x[i],to=angle+x[i],length.out = N))
		r = c(seq(from=0,to=radius,length.out = N),
					rep(radius,N),
					seq(from=radius,to=0,length.out = N))
		color = col[((i-1) %% length(col)) + 1]
		polygon(getPolar(a,r,r*yradiusscale,center),border = border,col = color,lty=1,lwd=1)
		if(!is.null(labels)){
			a = rep(angle+spiral.shift+x[i]/2,2)
			r = c(radius,radius + tck.length)
			tck = getPolar(a,r,r*yradiusscale,center)
			lines(tck,col=color)
			t = getPolar(a[1],r[2]+tck.length/3,(r[2]+tck.length/3)*yradiusscale,center)
			text(t$x,t$y,labels[i],adj=c(0.5-cos(a[1])/2,0.5-sin(a[1])/2))
		}
		angle = angle + x[i]
	}
}
