var FormattedNumber = ReactIntl.FormattedNumber;

var SellerForm = React.createClass({
	handleSubmit: function(e){
		e.preventDefault();
		var name = this.refs.name.getDOMNode().value.trim();
		var password = this.refs.password.getDOMNode().value.trim();
		var url = this.refs.url.getDOMNode().value.trim();

		if(!name || !url) {
			return;
		}

		this.props.onSellerSubmit({name:name, password:password, url:url});

		this.refs.name.getDOMNode().value= '';
		this.refs.password.getDOMNode().value= '';
		this.refs.url.getDOMNode().value= '';
	},

	render: function(){
		return (
			<div className='jumbotron'>
				<h2>Hello, Seller!</h2>

				<form className='form-inline' onSubmit={this.handleSubmit}>
					<div className='form-group'>
						<label htmlFor='name' className='sr-only'>Name</label>
						<input type='text' placeholder='your name' className='form-control' ref='name'
                               data-toggle='tooltip' data-placement='bottom' title='Your username'/>
					</div>
					<div className='form-group'>
						<label htmlFor='password' className='sr-only'>Password</label>
						<input type='password' placeholder='your password' className='form-control' ref='password'
                               data-toggle='tooltip' data-placement='bottom' title='Password is used if you want to register yourself on a different url. You will need to provide the same username with the same password. Beware that there is nothing that can be done to retrieve it...'/>
					</div>
					<div className='form-group'>
						<label htmlFor='url' className='sr-only'>URL</label>
                        <input type='text' placeholder='http://192.168.1.1:3000' className='form-control' ref='url'
                               data-toggle='tooltip' data-placement='bottom' title='Base url of your own client' />
					</div>
					<button type='submit' className='btn btn-success'>Register</button>
				</form>
			</div>
		);
	}
});

var SellerView = React.createClass({
	getInitialState: function() {
		return {data: {}};
	},
	string2Color: function(str) {
		var hash = 0;

		for (var i = 0; i < str.length; i++) {
			hash = str.charCodeAt(i) + ((hash << 5) - hash);
		}

		var color = "#";

		for (var j = 0; j < 3; j++) {
			color += ("00" + ((hash >> j * 8) & 0xFF).toString(16)).slice(-2);
		}

		return color;
	},
	formatChartData: function(data) {
		var chartData = {};
		var labels = [];
		var datasets = [];
		if(data && !_.isEmpty(data.history)) {
			var seller;
			for (seller in data.history) {
				var color = this.string2Color(seller);
				datasets.push({
					label: seller,
					fillColor: "transparent",
					strokeColor: color,
					pointColor: color,
					pointStrokeColor: "#fff",
					pointHighlightFill: "#000",
					pointHighlightStroke: color,
					data: _.takeRight(data.history[seller], 10)
				});
			}

			var lastIteration = data.lastIteration;
			for(var i = 0; i < lastIteration; i += 10) {
				labels.push(i + "");
			}
		}else  {
			return undefined;
		}

		chartData['datasets'] = datasets;
		chartData['labels'] = _.takeRight(labels, 10);
		return chartData;
	},
	componentDidMount: function() {
		var ctx = document.getElementById("salesChart").getContext("2d");
		var chart = new Chart(ctx);
		this.setState({chart: chart});
	},
	componentWillReceiveProps: function() {
		var history = this.formatChartData(this.props.salesHistory);
		this.setState({salesHistory: history});
	},
	refreshChart: function() {
		if(this.state.chart && this.state.salesHistory) {
			var chart = this.state.chart;
			var noAnimation = {
				bezierCurve: false,
				animation: false
			};
			chart.Line(this.state.salesHistory, noAnimation);
		}
	},
	render: function(){
		this.refreshChart();
		var self = this;
		var sellerNodes = this.props.data.map(function(seller) {
			var sellerColor = self.string2Color(seller.name);
			var showOfflineWarning = !seller.online ? <span title="offline" className="glyphicon glyphicon-alert" aria-hidden="true"></span> : '';
			return (
				<tr key={seller.name} style={{color: sellerColor}}>
					<td className="col-md-6"><strong>{seller.name}</strong></td>
					<td className="col-md-5">
						<FormattedNumber
							value={seller.cash}
							style='currency'
							currency='EUR' />
					</td>
					<td className="col-md-1">
						{showOfflineWarning}
					</td>
				</tr>
			);
		});
		return (
			<div>
				<div className='row'>
					<div className='col-md-4'>
						<h2>Ranking</h2>
						<div className='table-responsive'>
							<table className='table table-striped'>
								<thead>
								<tr>
									<th>Name</th>
									<th>Cash</th>
									<th></th>
								</tr>
								</thead>
								<tbody>
								{sellerNodes}
								</tbody>
							</table>
						</div>
					</div>
					<div className='col-md-8'>
						<h2>History</h2>
						<canvas id="salesChart" width="730" height="400"></canvas>
					</div>
				</div>
				<hr/>
				<footer>
					<p>Have fun :D </p>
				</footer>
			</div>
		);
	}
});

var Seller = React.createClass({
	loadSellersFromServer: function(){
		$.ajax({
			url: '/sellers',
			datatype: 'json',
			success: function(data) {
				this.setState({data: data});
			}.bind(this),
			error: function(xhr, status, err){
				console.error(this.props.url, status, err.toString());
			}.bind(this)
		});
	},

	getSalesHistory: function() {
		$.ajax({
			url: '/sellers/history?chunk=' + this.props.historyFrequency,
			datatype: 'json',
			success: function(data) {
				this.setState({salesHistory:data});
			}.bind(this),
			error: function(xhr, status, err){
				console.error(this.props.url, status, err.toString());
			}.bind(this)
		});
	},

	handleSellerSubmit: function(newSeller) {
		var currentSellers = this.state.data;
		var sellers = currentSellers.concat([newSeller]);

		$.ajax({
			url: this.props.url,
			datatype: 'json',
			type: 'POST',
			data: newSeller,
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

	reloadSellersData: function() {
		this.loadSellersFromServer();
		this.getSalesHistory();
	},

	componentDidMount: function(){
		this.reloadSellersData();
		setInterval(this.reloadSellersData, this.props.pollInterval);
	},

	render: function(){
		return (
			<div className='container'>
				<SellerForm onSellerSubmit={this.handleSellerSubmit} />
				<SellerView data={this.state.data} salesHistory={this.state.salesHistory} />
			</div>
		);
	}
});

React.render(
	<Seller url='/seller' pollInterval='5000' historyFrequency='10' />,
	document.getElementById('seller')
);

$(function () {
    $('[data-toggle="tooltip"]').tooltip()
});
