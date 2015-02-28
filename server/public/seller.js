var SellerForm = React.createClass({
	handleSubmit: function(e){
		e.preventDefault();
		var name = this.refs.name.getDOMNode().value.trim();		
		var url = this.refs.url.getDOMNode().value.trim();	
		if(!name || !url){
			return;
		}	

		this.props.onSellerSubmit({name:name, url:url});

		this.refs.name.getDOMNode().value= "";
		this.refs.url.getDOMNode().value= "";
	},
	render: function(){
		return (
			<div className="jumbotron">
				<div className="container">
	        		<h1>Hello, Seller!</h1>
					<form className="navbar-form" onSubmit={this.handleSubmit}>
			            <p className="form-group">Add seller name</p>
			            <div className="form-group">
			              <input type="text" placeholder="your name" className="form-control" ref="name" />
			            </div>
			              <p className="form-group">Add seller url</p>
			            <div className="form-group">
			              <input type="text" placeholder="http://192.168.1.1:3000" className="form-control" ref="url" />
			            </div>
			            <button type="submit" className="btn btn-success">Register</button>
		          	</form>
		         </div>
	         </div>
		);
	}
});

var SellerView = React.createClass({

	render: function(){
		var sellerNodes = this.props.data.map(function(seller){
			return (
				<tr>
                  <td>{seller.name}</td>
                  <td>{seller.cash}</td>
                </tr>
			);
		});
		return (
			<div className="container">
		        <div className="row">
		          	<div className="col-md-6">
		            	<h2>Sellers ranking</h2>
		            	<div className="table-responsive">
				            <table className="table table-striped">
				              <thead>
				                <tr>
				                  <th>Name</th>
				                  <th>Cash</th>
				                </tr>
				              </thead>
				              <tbody>
				                {sellerNodes}
				              </tbody>
				            </table>
				          </div>
		        	</div>
		         	<div className="col-md-6">
		            	<h2>Sellers evolution</h2>
		            	<p>Donec id elit non mi porta gravida at eget metus. Fusce dapibus, tellus ac cursus commodo, tortor mauris condimentum nibh, ut fermentum massa justo sit amet risus. Etiam porta sem malesuada magna mollis euismod. Donec sed odio dui. </p>
		        	</div>
		        </div>
		        <hr/>
		        <footer>
			      <p>&copy; Diego &amp; Radwane </p>
			    </footer>
		    </div>
		);
	}
});

var Seller = React.createClass({
	loadSellersFromServer: function(){
		$.ajax({
			url:"/sellers",
			datatype:'json',
			success:function(data){
				this.setState({data: data});
			}.bind(this),
			error: function(xhr, status, err){
				console.error(this.props.url, status, err.toString());
			}.bind(this)
		});
	},
	handleSellerSubmit: function(seller) {
		var currentSellers = this.state.data;
		var sellers = currentSellers.concat([seller]);
		$.ajax({
			url: this.props.url,
			datatype: 'json',
			type: 'POST',
			data: seller,
			success: function(){
				this.setState({data:sellers});
			}.bind(this),
			error: function(xhr, status, err){
				console.error(this.props.url, status, err.toString());
			}.bind(this)
		});
	},
	getInitialState: function(){
		return {data: []};
	},
	componentDidMount: function(){
		this.loadSellersFromServer();
		setInterval(this.loadSellersFromServer, this.props.pollInterval);
	},
	render: function(){
		return (
			<div className="container">
				<SellerForm onSellerSubmit={this.handleSellerSubmit} />
				<SellerView data={this.state.data} />
			</div>
		);
	}
});

React.render(
	<Seller url="/seller" pollInterval="5000" />,
	document.getElementById('seller')
);