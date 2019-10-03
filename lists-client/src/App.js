import React from 'react';
import './App.css';
import Register from './components/register';
import Login from './components/login';
import Header from './components/header';
import { StateProvider } from './state/stateContext';
import { BrowserRouter as Router, Route} from 'react-router-dom';
import Lists from './components/lists';
import Items from './components/items';

function App() {
  return (
    <div className="App">
      <Router>
        <StateProvider>
          <div>
            <Header />
            <div>
              <Route path="/register" component={Register} />
              <Route path="/login" component={Login} />
              <Route path={["/list/:id", "/lists"]} component={Lists} />
	            <Route path={["/items/:id", "/items"]} component={Items} />
	  
	      
            </div>
          </div>
        </StateProvider>
      </Router>
    </div>
  );
}

export default App;
