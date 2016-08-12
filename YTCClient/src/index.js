/* @flow */
import React, { Component } from 'react';
import {
  StyleSheet,
  Text,
  View,
  ListView,
  Linking,
  Modal,
  TextInput,
  TouchableHighlight,
  AsyncStorage
} from 'react-native';

type URL = string;

type Cast = {
  castTitle: string,
  castPath: URL
};

type YTCClientDefaultProps = void;

type YTCClientProps = void;

type YTCClientState = {
  baseUrl: ?string,
  castsDS: ListView.DataSource,
  showError: boolean,
  errorMsg: ?string
}

export default class YTCClient extends Component {
  static defaultProps: YTCClientDefaultProps;

  props: YTCClientProps;

  state: YTCClientState;

  constructor(props: YTCClientProps) {
    super(props);

    this.state = {
      castsDS: new ListView.DataSource({
        rowHasChanged: (a, b) => a !== b
      }),
      showError: false,
      baseUrl: null,
      errorMsg: null
    };

    AsyncStorage.getItem('baseUrl')
      .then((savedBaseUrl) => {
        this.setState({
          baseUrl: savedBaseUrl
        })
      });
  }

  _getUrl = (path: string) => {
    const { baseUrl } = this.state;
    if (!baseUrl) {
      return;
    }
    let url = `${baseUrl}${path}`;
    if (!/^http:\/\//.test(url)) {
      url = `http://${url}`;
    }
    return url;
  }

  _fetchCasts = () => {
    const url = this._getUrl('/casts');
    if (!url) {
      return;
    }
    fetch(url)
      .then((res) => res.json())
      .then((casts) => this.setState({
        castsDS: this.state.castsDS.cloneWithRows(casts)
      }))
      .catch(err => {
        this.setState({
          showError: true,
          errorMsg: "Did you even try to give me a real address?"
        })
      });
  };

  _getHandleCastPress = (path: string) => {
    return () => {
      const url = this._getUrl(path);
      if (!url) {
        return;
      }
      Linking.openURL(url)
        .catch(err => {
          this.setState({
            showError: true,
            errorMsg: "Holy shit, can you reallly not open a URL?"
          })
        });
    };
  }

  _handleDismissModal = () => {
    this.setState({
      showError: false
    })
  }

  _handleAddressEndEditing = (event: any) => {
    const baseUrl =  event.nativeEvent.text;
    AsyncStorage.setItem('baseUrl', baseUrl);
    this.setState({
      baseUrl
    });
  }

  componentDidUpdate(prevProps: YTCClientProps, prevState: YTCClientState) {
    if (prevState.baseUrl !== this.state.baseUrl) {
      this._fetchCasts();
    }
  }

  componentDidMount() {
    this._fetchCasts();
  }

  render() {
    return (
      <View style={styles.container}>
        <Modal
          animationType='fade'
          transparent={false}
          visible={this.state.showError}>
          <View style={{marginTop: 50}}>
            <View style={styles.container}>
              <Text style={styles.welcome}>{this.state.errorMsg}</Text>

              <TouchableHighlight onPress={this._handleDismissModal}>
                <Text style={styles.welcome}>Okay</Text>
              </TouchableHighlight>

            </View>
          </View>
        </Modal>
        <TextInput
          style={styles.textInput}
          autoCapitalize="none"
          autoCorrect={false}
          placeholder={this.state.baseUrl ? `using saved url: ${this.state.baseUrl}` : "enter casts api address e.g. http://localhost:3000"}
          onEndEditing={this._handleAddressEndEditing}
          keyboardType="url"
        />
        <Text style={styles.welcome}>
          YTCasts
        </Text>
        <ListView
          dataSource={this.state.castsDS}
          renderRow={(cast: Cast) =>
            <View style={styles.cast}>
              <Text style={styles.castTitle} onPress={this._getHandleCastPress(cast.castPath)}>
                {cast.castTitle}
              </Text>
            </View>
          }
        />
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    padding: 10,
    flex: 1,
    backgroundColor: '#F5FCFF',
  },
  welcome: {
    fontSize: 20,
    textAlign: 'center',
    margin: 10,
  },
  modal: {
    justifyContent: 'center'
  },
  textInput: {
    marginTop: 20,
    height: 30,
    borderWidth: 0.5,
    borderColor: '#0f0f0f',
    fontSize: 14,
    padding: 5
  },
  cast: {
    justifyContent: 'center',
    borderBottomColor: '#DDD',
    borderBottomWidth: 2,
    minHeight: 50,
    marginTop: 5,
    marginBottom: 5,
  },
  castTitle: {
    fontSize: 16
  }
});
