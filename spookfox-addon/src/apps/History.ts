import { Draft, Immutable } from 'immer';
import browser from 'webextension-polyfill';
import { SFApp, Spookfox } from '~src/Spookfox';

export type HistoryState = Immutable<null>;

export interface SFHistoryItem {
  id: string;
  url: string;
  title: string;
  lastVisitTime: number;
  visitCount: number;
}

export default class History implements SFApp<HistoryState> {
  initialState: Immutable<HistoryState> = null;

  get state(): HistoryState {
    return this.sf.state[this.name];
  }

  dispatch(name: Actions, payload: unknown) {
    return this.sf.dispatch(`${this.name}/${name}`, payload);
  }

  constructor(
    public name: string,
    public sf: Spookfox
  ) {
    sf.registerReqHandler(EmacsRequests.SEARCH_HISTORY, this.searchHistory);
  }

  serializeHistoryItem(item: browser.History.HistoryItem): SFHistoryItem {
    return {
      id: item.id,
      url: item.url || '',
      title: item.title || '',
      lastVisitTime: item.lastVisitTime || 0,
      visitCount: item.visitCount || 0,
    };
  }

  /**
   * Search browser history by text query
   */
  searchHistory = async (
    msg: {
      text?: string;
      maxResults?: number;
      startTime?: number;
      endTime?: number;
    } = {}
  ): Promise<SFHistoryItem[]> => {
    const { text = '', maxResults = 1000, startTime, endTime } = msg;

    const searchQuery: browser.History.SearchQuery = {
      text,
      maxResults,
    };

    if (startTime) searchQuery.startTime = startTime;
    if (endTime) searchQuery.endTime = endTime;

    try {
      const historyItems = await browser.history.search(searchQuery);

      // Sort by frecency (frequency + recency score)
      const now = Date.now();
      const sortedItems = historyItems.sort((a, b) => {
        const aFrecency = (a.visitCount || 0) * Math.log(1 + (now - (a.lastVisitTime || 0)) / (1000 * 60 * 60 * 24));
        const bFrecency = (b.visitCount || 0) * Math.log(1 + (now - (b.lastVisitTime || 0)) / (1000 * 60 * 60 * 24));
        return bFrecency - aFrecency;
      });

      return sortedItems.map(this.serializeHistoryItem);
    } catch (error) {
      throw new Error(`History search failed: ${error.message}`);
    }
  };

  /**
   * Initialize the state.
   */
  init = async () => {
    this.dispatch(Actions.INIT, null);
  };

  reducer(_, state: Draft<HistoryState>) {
    return state;
  }
}

export enum Actions {
  INIT = 'INIT',
}

export enum EmacsRequests {
  SEARCH_HISTORY = 'H_SEARCH_HISTORY',
}
