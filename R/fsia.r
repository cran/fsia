
read.formscanner<-function(file,col.names=NULL,conc=NULL,id=NULL,dummy=NULL)
{
	data<-read.csv2(file,as.is=TRUE,na.strings = "")
	if (!is.null(col.names)) colnames(data)<-col.names #modified
	if (is.numeric(conc)) {
		tmp<-c()
		for (i in conc) tmp<-paste(tmp,data[,i],sep="")
		data[,conc[1]]<-tmp
		data<-data[,-(conc[2:length(conc)])]
	}
	if (!is.numeric(conc)) {
		tmp<-c()
		for (i in conc) tmp<-paste(tmp,data[,i],sep="")
		data[,conc[1]]<-tmp
		for (i in 2:length(conc)) data[,conc[i]]<-c()
	}
	if (!is.null(id)) colnames(data)[colnames(data)==id]<-"id"
	if (!is.null(dummy)) { #added option dummy
		if (is.numeric(dummy)) dummy<-colnames(data)[dummy]
		for (i in dummy) {
			dsp<-strsplit(data[,i],"[|]")
			opt<-sort(unique(unlist(dsp)))
			if (length(opt)>1) {
				dummydata<-matrix(unlist(lapply(dsp,FUN=function(x,opt) opt%in%x, opt=opt)),ncol=length(opt),byrow=TRUE)*1
				colnames(dummydata)<-paste(i,opt,sep=".")
				data<-cbind(data,dummydata)
			}
		}
	}
	return(data)
}


addkey<-function(obj,keyline=NULL,keyfile=NULL,keydata=NULL)
{
	if (is.null(keyline) & is.null(keyfile) & is.null(keydata)) stop("Specify keyline or keyfile or keydata.\n")
	if ((!is.null(keyline) + !is.null(keyfile) + !is.null(keydata)) > 1) stop("Specify only one key.\n")
	if (is.null(obj$key) & is.null(obj$weights)) data<-obj else data<-obj$data
	weights<-obj$weights
	if (!is.null(keyline)) {
		key<-data[keyline,]
		data<-data[-keyline,]
	}
	if (!is.null(keyfile)) key<-read.csv2(keyfile)
	if (!is.null(keydata)) key<-keydata
	sel<-colnames(key)[colnames(key)%in%colnames(data)]
	key<-key[,sel]
	if (is.null(weights)) obj<-list(data=data,key=key)
	else obj<-list(data=data,key=key,weights=weights)
	return(obj)
}

addweights<-function(obj,weightsfile=NULL,weightsdata=NULL)
{
	if (is.null(weightsfile) & is.null(weightsdata)) stop("Specify weightsfile or weightsdata.\n")
	if ((!is.null(weightsfile) + !is.null(weightsdata)) > 1) stop("Specify either weightsfile or weightsdata.\n")
	if (is.null(obj$key) & is.null(obj$weights)) data<-obj else data<-obj$data
	key<-obj$key
	if (!is.null(weightsfile)) weights<-read.csv2(weightsfile)
	if (!is.null(weightsdata)) weights<-weightsdata
	rownames(weights)<-weights$response
	sel<-colnames(weights)[colnames(weights)%in%colnames(data)]
	weights<-weights[,sel]
	if (is.null(key)) obj<-list(data=data,weights=weights)
	else obj<-list(data=data,key=key,weights=weights)
	return(obj)
}


resp2binary<-function(obj,columns)
{
	key<-obj$key
	if (is.null(key)) stop("key is required.\n")
	if (is.numeric(columns)) item<-colnames(obj$data)[columns]
	else item<-columns
	data<-obj$data
	out<-matrix(NA,nrow(data),length(columns))
	for (i in 1:length(columns)) {
		out[,i]<-(as.character(data[,item[i]])==as.character(key[,item[i]]))*1
	}
	data[,columns]<-out
	return(data)
}

resp2scores<-function(obj,columns)
{
	weights<-obj$weights
	if (is.null(weights)) stop("weights are required.\n")
	if (is.numeric(columns)) item<-colnames(obj$data)[columns]
	else item<-columns
	data<-obj$data
	out<-matrix(NA,nrow(data),length(columns))
	if (nrow(weights)==1) {
		key<-obj$key
		if (is.null(key)) stop("key is required.\n")
		for (i in 1:length(columns)) {
			out[,i]<-(as.character(data[,item[i]])==as.character(key[,item[i]]))*1*weights[1,i]
		}
	}
	if (nrow(weights)>1) {
		for (i in 1:length(columns)) {
			datsp<-strsplit(data[,item[i]],"[|]")
			for (j in 1:length(datsp)) out[j,i]<-sum(weights[datsp[[j]],item[i]])
		}
	}
	
	data[,columns]<-out
	return(data)
}


freq<-function(obj,columns,perc=FALSE)
{
	if (is.null(obj$key) & is.null(obj$weights)) data<-obj else data<-obj$data
	if (is.numeric(columns)) item<-colnames(data)[columns]
	else item<-columns
	if (!is.null(obj$key)) key<-as.matrix(obj$key[,item])
	else key<-obj$key
	out<-list()
	j<-1
	for (i in columns) {
		tab<-table(data[,i])
		if (perc) {
			tab<-tab/nrow(data)*100
			tab<-round(tab,2)
			#tab<-paste(tab,"%",sep="")
		}
		out[[j]]<-list(item=item[j],tab=tab,key=key[j])
		j<-j+1
	}
	class(out)<-"frlist"
	return(out)
}


print.frlist<-function(x, ...)
{
	for (i in 1:length(x)) {
		cat("\n============== ")
		cat(x[[i]]$item)
		cat(" ==============\n")
		tab<-x[[i]]$tab
		names(tab)[names(tab)==x[[i]]$key]<-paste(names(tab)[names(tab)==x[[i]]$key],"*",sep="")
		print(tab)
	}
	cat("\n")
}

plot.frlist<-function(x, display=TRUE, ask=TRUE, ...)
{
	devAskNewPage(ask = ask)
	for (i in 1:length(x)) {
		tab<-x[[i]]$tab
		colour<-rep(2,dim(tab))
		if (!is.null(x[[i]]$key)) colour[names(tab)==x[[i]]$key]<-3
		bp<-barplot(tab,col=colour,main=x[[i]]$item,ylim=c(0,max(tab)*1.2))
		if (display) {
			text(x=bp,y=(tab+max(tab)*0.02),labels=tab,adj = c(0.5, 0))
		}
	}
	devAskNewPage(ask = FALSE)
}




person.stat<-function(obj,columns,weights=FALSE)
{
	if (is.numeric(columns)) item<-colnames(obj$data)[columns]
	else item<-columns
	if (!weights) data01<-resp2binary(obj=obj,columns=columns)
	else data01<-resp2scores(obj=obj,columns=columns)
	score<-rowSums(data01[,columns],na.rm=TRUE)
	if (weights) count<-sum(apply(obj$weights[,item],2,FUN=function(x) sum(x[x>0])))
	else count<-length(columns)
	if (any(colnames(obj$data)=="id"))
		out<-data.frame(id=obj$data$id,score=score,max=count,perc=round(score/count,2))
	else
		out<-data.frame(rownames=rownames(obj$data),score=score,max=count,perc=round(score/count,2))
	return(out)
}



item.stat<-function(obj,columns,weights=FALSE)
{
	if (is.numeric(columns)) item<-colnames(obj$data)[columns]
	else item<-columns
	if (!weights) data01<-resp2binary(obj=obj,columns=columns)
	else data01<-resp2scores(obj=obj,columns=columns)
	score<-colSums(data01[,columns],na.rm=TRUE)
	count<-nrow(data01)
	if (weights) max<-apply(obj$weights[,item],2,FUN=function(x) sum(x[x>0]))*count
	else max=count
	out<-data.frame(item=names(score),score=score,max=max,perc=round(score/count,2))
	rownames(out)<-NULL
	return(out)
}



report<-function(obj,columns,whichid,grid=TRUE,main="",las=0,itemlab=NULL,weights=FALSE)
{
	if (!any(colnames(obj$data)=="id")) stop("id variable is missing. Select id in function read.formscanner.\n")
	if (is.numeric(columns)) item<-colnames(obj$data)[columns]
	else item<-columns
	n<-length(columns)
	resp<-as.matrix(obj$data[obj$data$id%in%whichid,columns])
	if (is.null(itemlab)) itemlab <- item
	nid<-length(whichid)
	if (!weights) {
		if (!is.null(obj$key)) key<-as.matrix(obj$key[,item])
		else key<-obj$key
		plot(1,ylim=c(0,n),xlim=c(0.5,nid+2+0.5),type="n",xaxt="n",yaxt="n",bty="n",ann=FALSE,main=main,las=las)
		axis(1,at=1:(nid+2),labels=c("item",whichid,"key"),tick=FALSE)
		text(1,n:1-0.5,itemlab)
		for (i in seq_along(whichid)) {
			colour<-rep(2,n)
			colour[resp[i,]==key]<-3
			text(i+1,n:1-0.5,resp[i,],col=colour)
		}
		text(nid+2,n:1-0.5,key)
	}
	if (weights) {
		if (nrow(obj$weights)==1) {
			if (!is.null(obj$key)) key<-as.matrix(obj$key[,item])
			else key<-obj$key
			plot(1,ylim=c(0,n),xlim=c(0.5,nid+2+0.5),type="n",xaxt="n",yaxt="n",bty="n",ann=FALSE,main=main,las=las)
			axis(1,at=1:(nid+2),labels=c("item",whichid,"weights"),tick=FALSE)
			text(1,n:1-0.5,itemlab)
			for (i in seq_along(whichid)) {
				colour<-rep(2,n)
				colour[resp[i,]==key]<-3
				wght<-obj$weights[,item]
				wght[resp[i,]!=key]<-0
				text(i+1,n:1-0.5,paste(resp[i,],wght,sep="="),col=colour)
			}
			text(nid+2,n:1-0.5,paste(key,obj$weights[,item],sep="="))
		}
		if (nrow(obj$weights)>1) {
			plot(1,ylim=c(0,n),xlim=c(0.5,nid+2+0.5),type="n",xaxt="n",yaxt="n",bty="n",ann=FALSE,main=main,las=las)
			axis(1,at=1:(nid+2),labels=c("item",whichid,"weights"),tick=FALSE)
			text(1,n:1-0.5,itemlab)
			weights<-as.matrix(obj$weights[,item])
			text(1,n:1-0.5,item)
			for (j in 1:length(columns)) {
				datsp<-strsplit(resp[,j],"[|]")
				for (i in 1:length(datsp)) {
					respij<-datsp[[i]]
					respij<-paste(respij,weights[respij,item[j]],sep="=")
					paste(respij,collapse="; ")
					text(i+1,n-j+0.5,paste(respij,collapse="; "))
					wght<-paste(rownames(weights),weights[,item[j]],sep="=")
					text(nid+2,n-j+0.5,paste(wght,collapse="; "))
				}
			}
		}
	}
	if (grid) abline(h=(0:n))
}


