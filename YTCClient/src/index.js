/* @flow */
import React, { Component } from 'react';
import {
  StyleSheet,
  Text,
  View,
  ListView,
  Linking,
  Modal
} from 'react-native';

type URL = string;

type Cast = {
  castTitle: string,
  castPath: URL
};

export default class YTCClient extends Component {
  constructor(props) {
    super(props);

    this.state = {
      castsDS: new ListView.DataSource({
        rowHasChanged: (a, b) => a !== b
      }),
      showError: false
    };

    const baseUrl = 'http://localhost:3000';

    fetch(`${baseUrl}/casts`)
      .then((res) => res.json())
      .then((casts) => this.setState({
        castsDS: this.state.castsDS.cloneWithRows(casts)
      }));

    this._getHandleCastPress = (path: string) => () => {
      Linking.openURL(`${baseUrl}${path}`)
        .catch(err => {
          this.setState({
            showError: true,
            castClickError: err
          })
        });
    }
  }

  render() {
    return (
      <View style={styles.container}>
        <Modal
          animationType='fade'
          transparent={true}
          visible={this.state.showError}>
          <Text>Holy shit, can you reallly not open a URL?</Text>
        </Modal>
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
